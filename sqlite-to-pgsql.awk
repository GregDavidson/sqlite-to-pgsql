#!/usr/local/bin/gawk -f
# Purpose: SQLite to PostgreSQL Database Converter
# Version: 0.5 of Monday 17 March 2014
# Author: J. Greg Davidson, http://jgd.ngender.net
# Copyright: GNU Affero General Public License,
# 	http://www.gnu.org/licenses/agpl-3.0.html
# Implementation notes:
# 	Intended to be run from the bash script
# 	 	sqlite-to-pgsql
# 	So far only tested with tables from Firefox 27
# 	To return values via an array parameter
# 	 	The array must be defined before being passed
# 	 	But Awk has no direct way of defining an array
# 	 	This idiom will do it:
# 	 	 	 my_array[""]=1; delete my_array[""]
# 	 	To define a subarray at index i of an array my_array:
# 	 	 	define_subarray(my_array, i)
# 	Some Mozilla sqlite table fields have missing types!!!
# 	 	These have been put in by hand with _find_table_field_type[]
# 	Globals managed by functions have names beginning with  _
# Variable Parameters:
# 	-v verbose=1 turns on verbose tracing to stderr
# 	-v only_print= comma  separated list of any of:
# 	 	tables = print CREATE TABLE statements
# 	 	inserts = print INSERT INTO statements
# 	 	indices = print CREATE INDEX statements
# 	 	transactions = print BEGIN and COMMIT statements
# 	 	input = print commented-out input code
# 	 	types = print unsorted list of field types at the END
# 	 	props = print unsorted list of field properties at the END
# 	-v schema=<schema-name>[?!@] puts everything in given schema
# 	 	 	? means create if does not exist
# 	 	 	! means drop if exists, then re-create schema
# 	 	 	@ means do these things inside the transaction (wait for a begin)
# Testing:
# 	We need more tests!!
# 	Study failed() and run_tests() to see testing context
# Conventions:
#	 	Things needing attention have a ?? or !! comment
#	 	Global array names begin with leading _
#	 	Global scalar names mostly begin with _ or default_
#	 	Firebreak parameter _ separates real parameters from locals
# 	 	Put in checks that these are not set??

## Some Simple String Functions

function alt(x, alternative) { return x ? x : alternative; }
function alt_at(a, i, alternative) {	return (i in a) ? a[i] : alternative; }
function pair(x, y, separator) {
		return (x) (x&&y ? alt(separator, " ") : "") y
}
function trim(s) {
		return gensub( /^\s+/, "", "1", gensub(/\s+$/, "", "1", s) )
}
function open_close(opener, closer) {
		if (closer) return closer
		switch (opener) {
		case "[": return "]" ;;
		case "<": return ">" ;;
		case "`": return "'" ;;
		case " ": return "" ;;
		default: return ")" ;;
		}
}
function wrap(x, label, left, right) {
		return alt(label, "") alt(left, "(") x open_close(left, right)
}
function rap(x, label, left, right) { return wrap(x, label, "[", "]"); }

## Debugging

function context() {
		if (!FILENAME) return ""
		return sprintf("-- %s\n-- %s[%d] ", $0, FILENAME, FNR)
}
function exception(level, msg, code, _, output) {
		output = context() pair(level, msg, ": ")
		print output >"/dev/stderr" ;  print output
		if (code) { exit code; } else { return 0; }
}
function error(msg, code) { exception("Error", msg, code); }
function failed(bool, what, msg, code, is_assert) {
		if (bool) {	++_num_tests_passed_; return 1; }
		if (_test_error_ || is_assert)
				exception(pair(what, "failed"), msg, code)
		_test_error_ = 1
		return bool
}
function assert(bool, msg, code ) {
		return failed(bool, pair("Assertion", msg), "", alt(code, _assert_exit_code_), 1)
}

## Tracing

function here(place, value, label) {
		alt(value, label) ? do_show(value, "@" pair(this(place), label)) : do_show(this(place),  "@" )
		return 1
}
function set_debug(new, _, old) { old = _debug_; _debug_ = new; return old; }
function show_context() { if (_debug_) printf "%s", context() >"/dev/stderr"; }
function show_scalar(value, label) {
		if ( label ) printf "%s: ", label >"/dev/stderr"
		printf "%s\n", value >"/dev/stderr"
}
function do_show_element(a, i, label) {	do_show(a[i], rap(i, label)); }
function show_element(a, i, label) {	if ( _debug_ ) do_show_element(a,i,label); }
function show_array(array, label, _, i) {
		printf "%s:\n", pair("Array", this(label)) >"/dev/stderr"
		for ( i in array )
				do_show_element( array, i, label )
}
function do_show(value, label) {
		if ( isarray(value) )
				show_array(value, label)
		else
				show_scalar(value, label)
}
function show_(value, label) {	if ( _debug_ ) do_show(value, label); }
function enter(fn, value, label) {
		_stack[++_stack_ptr] = fn
		( length(value) || label ) ? show_(value, "-->" pair(fn, label)) : show_(fn,  "-->" )
}
function leave(value, label, _, fn) {
		fn = _stack_ptr ? _stack[_stack_ptr--] : ""
		( length(value) || label ) ? show_(value, "<--" pair(fn, label)) : show_(fn, "<--")
		return value
}
function this_() {	return _stack_ptr ? _stack[_stack_ptr] : ""; }
function this(label) { return pair(this_(), label, "."); }
function show(value, label) {	show_(value, this(label)); }
function show_at(value, i, label) { show_(value, this(rap(i, label))); }

## Array Management

function assert_array(x, name) { assert(isarray(x), this(name) " is array"); }
function vector_append(array, val) { array[length(array)+1] = val; }
function at_append(array, i, val, delim) { array[i] = pair(array[i], val); }

# given an array a, and index i, create a[i] as an array
# by creating and then removing element a[i][""]
function define_subarray(a, i, aname) {
		enter("define_subarray", rap(i, aname))
		assert_array(a, aname)
		a[i][""] = 1;	delete a[i][""] # array a[i]
		assert_array(a[i], rap(i, aname))
		leave()
}
function define_subarrays(a, subarrays, aname, _, i) {
		enter("define_subarrays", subarrays, "subarrays")
		for (i in subarrays)	define_subarray(a, subarrays[i], aname)
		leave()
}

## Testing

function test(bool, what, msg, code) {
		return failed(bool, alt(what, "Test"), msg, alt(code, _test_exit_code_))
}
function test_eq_scalars(x, y, xname, yname, msg, code) {
		return test( x == y, wrap(x, xname) "== " wrap(y, yname), msg, code )
}
function test_eq_arrays(x, y, xname, yname, msg, code, _, i) {
		if ( ! test_eq_scalars( length(x), length(y), wrap(xname, "length"), wrap(yname, "length"), msg, code ) )
				return 0
		for (i in x)
				if ( ! test_eq( x[i], y[i], rap(i, xname), rap(i, yname), msg, code ) )
						return 0
		return 1
}
function test_eq(x, y, xname, yname, msg, code) {
		xname = this(xname); yname = this(yname)
		assert( isarray(x) == isarray(y), \
						wrap(isarray(x), wrap(xname, "isarray"), "`") "== "		\
						wrap(isarray(y), wrap(yname, "isarray"), "`"), code )
		if ( isarray(x) )
				return test_eq_arrays(x, y, xname, yname, msg, code)
		else
				return test_eq_scalars(x, y, xname, yname, msg, code)
}

## Some Tests Of Some Preceeding Functions

function pair_tests() {
		enter("pair_tests")
		return leave( \
				test_eq( pair("x", "y", ":"), "x:y", "pair(\"x\", \"y\", \":\")" ) &&
				test_eq( pair("x", "y"), "x y", "pair(\"x\", \"y\"" ) &&
				test_eq( pair("x", "", ":"), "x", "pair(\"x\", \"\", \":\")" ) &&
				test_eq( pair("", "y", ":"), "y", "pair(\"\", \"y\", \":\")" ) )
}

function trim_tests() {
		enter("pair_tests");
		return leave( \
				test_eq( trim("   "), "", "trim(\"   \")" ) &&
				test_eq( trim("x"), "x", "trim(\"x\")" ) &&
				test_eq( trim(" x"), "x", "trim(\" x\")" ) &&
				test_eq( trim("x "), "x", "trim(\"x \")" ) &&
				test_eq( trim(" x "), "x", "trim(\" x \")" ) &&
				test_eq( trim(" x y "), "x y", "trim(\" x y \")" ) )
}

## Domain-Specific Functions

# Initialize _sq_keyword[], adjective[]
function sq_keyword_init(_, keywords) {
		# Keywords that can't be names or types
		patsplit("AUTOINCREMENT DEFAULT NOT NULL PRIMARY KEY UNIQUE",	keywords)
		vector_append(keywords, "NOT NULL")
		vector_append(keywords, "PRIMARY KEY")
		for ( kw in keywords ) _sq_keyword[keywords[kw]]=1
		# Initialize adjective table (values really just need to be "true")
		adjective["NOT"] = "NULL";	# NOT normally modifies NULL
		adjective["PRIMARY"] = "KEY" # PRIMARY normally modifies KEY
}
function sq_keyword(word) { return (word in _sq_keyword) }
function is_valid_bare_ident(s) {
		if ( sq_keyword(s) ) return 0;
		switch (s) {
		case /^[[:upper:]_][[:upper:][:digit:]_]*$/: # no lower case
		case /^[[:lower:]_][[:lower:][:digit:]_]*$/: # no upper case
				return 1 ;;
		case /^[[:alpha:]_][[:alnum:]_]*$/: # mixed case
				return 0 ;;
		default:
				exception("is_valid_bare_ident Warning", "impossible identifier ", s)
				return 0 ;;
		}
}
function is_valid_sqlite_ident(s) {
		if ( sq_keyword(s) ) return 0
		switch (s) {
		case /^[[:alpha:]_][[:alnum:]_]*$/:
		case /^"[[:alpha:]_][[:alnum:]_]*"$/:
				return 1 ;;
		default: return 0 ;;
		}
}
function is_safe_ident(s) {
		return match(s, /^"[[:alpha:]_][[:alnum:]_]*"$/) || is_valid_bare_ident(s)
}
function quote_ident(s) {
		return "\"" gensub(/"/, "\\\"", "g", s) "\""
}
# Warning: if s is already quoted, but does not need
# to be, we will leave it quoted, which could cause it
# to NOT match if it's unquoted somewhere else!!
function safe_ident(s) {
		if ( is_valid_bare_ident(s) ) return s
		switch (s) {
		case /^[[:alpha:]][[:alnum:]_]*$/: # mixed case
				return quote_ident(s) ;;
		case /^"[[:alpha:]][[:alnum:]_]*"$/: return s
		}
		error( "Bad identifier " s )
}

function array_to_ident_list(array, _, sep, i, end, result) {
		result = "("
		sep = ""
		end = length(array)
		for ( i = 1 ; i <= end ; i++ ) {
				result = result sep safe_ident(array[i])
				sep = ","
		}
		return result ")"
}

# The sqlite code is inserting numbers like
# 	1205808866861485
# into fields declared as
# 	lastModified INTEGER
# It's apparently microseconds since the Epoch:
# select to_timestamp(1205808866861485 / 1000000);
# --> 2008-03-17 19:54:26-07
function pg_type_init() {
		_pg_type["BLOB"] = "bytea"
		_pg_type["DATETIME"] = "timestamp"
		_pg_type["LONG"] = "bigint"
		_pg_type["INTEGER"] = "bigint"
		_pg_type["LONGVARCHAR"] = "text"
}
function pg_type(t) {
		switch (t) {
		case /^VARCHAR\([1-9][0-9]*\)$/: return "text"
		default: return alt_at(_pg_type, t, t)
		}
}
function pg_value(t, x) {
		switch ( toupper(t) ) {
		case "BOOL":
				switch (x) {
				case "NULL": return x
				case "'0'": case "0": return "false"
				default: return "true"
				}
		case "BLOB":	return gensub(/^X'/, "E'\\\\\\\\x", "1", x)
		case "DATETIME": return "to_timestamp(" x ")"
		default:	return x
		}
}
function pg_prop_init() {
		_pg_prop["AUTOINCREMENT"] = "-nil-"
}
# returning "-nil-" means NO property
function pg_prop(p) {	return alt_at(_pg_prop, p, p); }
function add_prop(props, i, prop, _, p) {
		p = pg_prop(prop)
		if ( p != "-nil-" )	props[i] = props[i] " " p
}

function scan_match(s, want) {
		switch (want) {
		case " ": return match(s, /[[:space:]('"]/)
		case ",": return match(s, /[,('"]/)
		case "'": return match(s, /['\\]/)
		case "\"": return match(s, /["\\]/)
		case ")": return match(s, /[()'"]/)
		default: return match(s, "[('\"" want "]")
		}
}
# Separate values by specified separator, e.g. commas or whitespace
# Values may include subexpressions nested in quotes or parentheses.
# Handle backslashed quotes within quotes.  Trim the results.
# Implemented as a nested FSM.  Outer machine has three nodes:
# Exit, UnNested, Nested using {level}.  Inner machines use {want}.
# { case /".../: } confuses Emacs, so using { case "\"":	case ... } instead
function parse_vals(array,s,sep,_,n,buf,buf_s,level,want,pos,s_cnt,s_pos,chr) {
		enter("parse_vals", s, "s")
		assert_array(array)
		show(sep, "sep")
		assert( sep , "parse_vals() separator required" )
		n = 0												# number of fields in array
		buf = ""										# portion of current field processed so far
		level = 0										# nesting level in current field
		want = sep									# drives the two inner FSMs
		while ( buf || s ) {
				pos = scan_match(s, want) # match next special character
				if ( pos == 0 ) {						# State: Exit
						buf_s = trim(buf s)
						if ( length(buf_s) ) array[++n] = buf_s
						buf = ""; 	s = "" ;	s_cnt = 0 ; s_pos = 0
						if ( level > 0 ) exception(level, "Premature EOS " s)
				} else {
						chr = substr(s, pos, 1)
						# default to moving all processed characters
						s_cnt = pos	;	s_pos = pos+1
						if ( !level ) {				# State: UnNested
								if ( chr == sep && sep != " " ) {
										array[++n] = trim( buf substr(s, 1, pos-1) )
										buf = "" ;	s_cnt = 0
								} else
										switch (chr) {
										case /[,[:space:]]/:
												assert( want == " ", "non-space want " want)
												array[++n] = trim( buf substr(s, 1, pos-1) )
												buf = "" ;	s_cnt = 0
												s = trim( substr(s, s_pos) ) ; s_pos = 0;
												break
								case "'":	case "\"":	case "(":
												++level ; want = chr=="(" ? ")" : chr
												break
										default:
												exception(wrap(want, level, "<"), "Impossible char " s);
										}
						} else {						# State: Nested (level deep)
								switch (chr) {
								case "\\":
										if ( want == ")" ) {
												exception(level, "Backslash outside quotes in " s)
										} else if ( pos == length(s) ) {
												exception(level, "Premature EOS " s)
										} else {
												s_cnt = pos+1 ; s_pos = pos+2
										}
										break
								case "'":	case "\"":
										if ( chr == want ) { # unnest a level
												--level ; want = level ? ")" : sep
										} else {
												++level ; want = chr
										}
										break
								case "(":
										if ( want == ")" )	++level
										break
								case ")":
											 if ( chr == want ) { # unnest a level
													 --level ; want = level ? ")" : sep
											 }
										break
								default:
										exception(wrap(want, level), "Impossible char " s);
								}
						}
				}

				buf && show(buf, "buf")
				show(s, wrap(n, "s"))
				show(chr, rap(pos, "s"))
				level && show(want, "want(" level ")")
				s_cnt != pos && show(s_cnt, "s_cnt");
				s_pos != pos+1 && show(s_pos, "s_pos");

				if (s_cnt) buf = buf substr(s, 1, s_cnt)
				s_cnt && show(buf, "buf")
				if (s_pos) s = substr(s, s_pos)
				s_pos && show(s, "(" n ")s")

		}
		show(array, "array")
		return leave(n, "n")
}

# string s encodes tokens separated by vertical bars
# the string to parse will have instances of delim instead
# a is the answer array to return, if desired
function parse_vals_test_(a, s, delim, _, b, s2) {
		enter("parse_vals_test_", s, "s")
		split(s, b, /\|/)						# the array we expect
		s2 = gensub(/\|/,delim,"g",s)
		test_eq( parse_vals(a, s2, delim), length(b), s )
		return leave( test_eq(a, b, s, "expected") )
}

function parse_vals_test(s,delim, _, a) {
		a[""]=1; delete a[""]				# array a
		return parse_vals_test_(a, s, delim)
}

function parse_vals_tests() {
		enter("parse_vals_tests")
		return leave( \
				parse_vals_test("", " ") &&
				parse_vals_test("", ",") &&
				parse_vals_test("PRIMARY|KEY|(place_id, input)", " ") &&
				parse_vals_test("keyword|VARCHAR(32)|PRIMARY|KEY|UNIQUE|NOT|NULL|DEFAULT|'foo'|AUTOINCREMENT", " ") &&
				parse_vals_test("place_id INTEGER NOT NULL|input LONGVARCHAR|PRIMARY KEY (place_id, input)", ",") &&
				parse_vals_test("id LONG PRIMARY KEY DEFAULT 0|keyWord LONGVARCHAR UNIQUE DEFAULT NULL", ",") &&
				parse_vals_test("id|LONG|PRIMARY|KEY|DEFAULT|0", " ") &&
				parse_vals_test("keyWord|LONGVARCHAR|UNIQUE|DEFAULT|NULL", " ") )
}

# Parse a table definition clause into parts, join adjectives
# with the parts which they modify to form terms.
# Return the number of terms in the result.
# Do we want to join "PRIMARY KEY" with following "(field_list,...)"??
function parse_clause(terms,clause,_,parts,num_parts,num_terms,p) {
		enter("parse_clause", clause, "clause");
		parts[""]=1; delete parts[""]	# array parts
		num_parts = parse_vals(parts, clause, " ")
		num_terms = 0
		for ( p=1 ; p <= num_parts ; ) {
				terms[++num_terms] = ""
				while ( (parts[p] in adjective) && p <= num_parts )
						at_append(terms, num_terms, parts[p++], " ")
				if ( p <= num_parts )
						at_append(terms, num_terms, parts[p++], " ")
		}
		show(terms, "terms")
		return leave(num_terms, "num_terms")
}

function parse_clause_test_(terms, s, _, num_terms, clause, b) {
		enter("parse_clause_test_")
		split(s, b, /[|]/)
		clause = gensub( /\|/, " ", "g", s)
		test_eq( parse_clause(terms, clause), length(b), wrap(s, "len"), "expected" )
		return leave( test_eq(terms, b, "terms", "expected") )
}

function parse_clause_test(s, _, terms) {
		terms[""]=1; delete terms[""]				# array terms
		return parse_clause_test_(terms, s)
}

# also used in parse_field_terms_test_1
function parse_clause_test_1() {
		return parse_clause_test("keyword|VARCHAR(32)|PRIMARY KEY|UNIQUE|NOT NULL|DEFAULT|'foo'|AUTOINCREMENT")
}

# also used in parse_field_terms_test_2
function parse_clause_test_2() {
		return parse_clause_test("keyWord|LONGVARCHAR|UNIQUE|DEFAULT|NULL")
}

function parse_clause_tests(t,_, v1, v2) {
		enter("parse_clause_tests")
		return leave( \
				parse_clause_test("") &&
				parse_clause_test_1() &&
				parse_clause_test_2() )
}

function find_table_field_type_init() {
		_find_table_field_type["moz_hosts"]["typed"] = "INTEGER"
		_find_table_field_type["moz_hosts"]["prefix"] = "TEXT"
}
function find_table_field_type(table_name, field_name, field_num, terms, num_terms, _, type) {
		if ( num_terms >= 2 && !sq_keyword(terms[2]) )
				type = terms[2]
		else if (field_name in _find_table_field_type[table_name]) {
				type = _find_table_field_type[table_name][field_name]
				exception("Kludge Warning", "No type for " table_name "." field_name ", substituting " type)
		} else	{
				type = "text"
				error("No type for " table_name "." field_name ", substituting text");
		}
		the_types[type]++
		return type
}

function	parse_field_terms(table_name, terms, field, field_names, field_types, field_defaults, field_props, field_prop_set, _, num_terms, t) {
		enter("parse_field_terms", terms)
		num_terms = length(terms)
		field_names[field] = terms[1]
		show_at(field_names[field], field, "field_names")
		field_types[field] = find_table_field_type(table_name, field_names[field], field, terms, num_terms)
		show_at(field_types[field], field, "field_types")
		for ( t = 3; t <= num_terms ; ) {
				if ( terms[t] == "DEFAULT" && t < num_terms ) {
						field_defaults[field] = pg_value(field_types[field], terms[t+1])
						show_at(field_names[field], field, "field_names")
						show_at(field_defaults[field], field, "field_defaults")
						t += 2
				} else {
						add_prop(field_props, field, terms[t])
						field_prop_set[terms[t]] = 1
						the_props[terms[t]]=1
						show_at(terms[t], field, "field_props")
						++t
				}
		}
		leave()
}

# a is an empty but defined tables array
# tables array b contains test clauses and expected results
# tests parse_field_terms for field f, filling and checking a
function parse_field_terms_test(a, table_name, b, f, _) {
		enter("parse_field_terms_test")
		a["clauses"][f] = b["clauses"][f]
		parse_field_terms( \
				table_name, a["clauses"][f], f, a["field_names"],	a["field_types"],
				a["field_defaults"],	a["field_props"],	a["field_prop_set"] )
		return leave( \
				test_eq( a["field_names"][f], b["field_names"][f] ) &&
				test_eq( a["field_types"][f], b["field_types"][f] ) &&
				test_eq( a["field_defaults"][f], b["field_defaults"][f] ) &&
				test_eq( a["field_props"][f], b["field_props"][f] ) &&
				test_eq( a["field_prop_set"][f], b["field_prop_set"][f] ) )
}

# test the field data set up in run_tests()
function parse_field_terms_tests(t, _, a, i) {
		enter("parse_field_terms_tests")
		define_subarray(t, "a", rap("a", "t"))
		define_subarrays(t["a"], _tables_subarrays_, rap("a", "_test_tables"))
		for ( i in t["a"]["clauses"] )
				if ( ! parse_field_terms_test( t["a"], "a", t["b"], i ) )
						leave(0)
		leave(1)
}

# this and it's calls reveal the tables structure
function test_table_set(t, n, f, s, field_name, field_type, field_default, field_props, field_prop_set, _, s0, bi, i) {
		enter("test_table_set", s, wrap(f, n))
		s0 = sprintf(s, field_name, field_type, field_default, field_props)
		define_subarray( t[n]["clauses"], f )
		parse_clause_test_( t[n]["clauses"][f], s0 )
		t[n]["field_names"][f] = field_name
		t[n]["field_types"][f] = field_type
		t[n]["field_defaults"][f] = field_default
		t[n]["field_props"][f] = gensub( /\|/, " ", "g", field_props )
		if (! field_prop_set) field_prop_set = field_props
		split(field_prop_set, bi, /[|]/)
		for (i in bi) t[n]["field_prop_set"][f][bi[i]] = 1
		show(t[n], wrap(n, "t"))
		leave()
}

function tables_init() {
		enter("tables_init")
		tables[""]=1; delete tables[""]	# array tables
		_test_tables[""]=1; delete _test_tables[""]	# array _test_tables
		define_subarray(_test_tables, "b", rap("b", "_test_tables"))
		define_subarrays(_test_tables["b"], _tables_subarrays_, rap("b", "_test_tables"))
		test_table_set(													 \
				_test_tables, "b", 1,																						\
				"%s|%s|DEFAULT|%s|%s|AUTOINCREMENT",														\
				"keyword", "VARCHAR(32)", "'foo'", "PRIMARY KEY|UNIQUE|NOT NULL" )
		test_table_set(																							\
				_test_tables, "b", 2, "%s|%s|DEFAULT|%s|%s",						\
				"keyWord", "LONGVARCHAR", "NULL", "UNIQUE", "UNIQUE" )
		leave()
}

function parse_table(table_name, clauses, field_of_clause, field_names, field_types, field_defaults, field_props, clause_terms, field_prop_set,_, num_clauses, num_terms, num_fields, c, c1) {
		enter("parse_table", clauses, "clauses")
		num_clauses = length(clauses)
		num_fields = 0
		for ( c=1 ; c <= num_clauses ; c++ ) {
				define_subarray(clause_terms, c)
				num_terms = parse_clause(clause_terms[c], clauses[c]);
				show(clause_terms[c], rap(c, "clause_terms"))
				c1 = clause_terms[c][1]
				show(c1,"clause_terms[c][1]")
				show(is_valid_sqlite_ident(c1),"is_valid_sqlite_ident(" c1 ")")
				if ( is_valid_sqlite_ident(c1) ) {
						num_fields++
						field_of_clause[c] = num_fields
						clause_of_field[num_fields] = c
						define_subarray(field_prop_set, num_fields)
						parse_field_terms(table_name, clause_terms[c], num_fields, field_names, field_types, field_defaults, field_props, field_prop_set[num_fields])
				}
		}
		leave()
}

function print_table_(table_name, clauses, field_of_clause, field_names, field_types, field_defaults, field_props, field_prop_set,_, c, num_clauses, f, d, p) {
		enter("print_table", table_name, "table_name")
		printf "CREATE TABLE %s (\n", safe_ident(table_name)
		num_clauses = length(clauses)
		for ( c=1 ; c <= num_clauses ; c++ ) {
				show(clauses[c], "clauses")
				f = field_of_clause[c]
				if (f) {
						printf "\t%s %s", safe_ident(field_names[f]), pg_type(field_types[f])
						if ( f in field_defaults ) {
								show(field_defaults[f], "default")
								printf " DEFAULT %s", pg_value(field_types[f], field_defaults[f])
						}
						printf "%s", field_props[f]
				} else
						printf "\t%s", clauses[c]
				printf "%s\n", c < num_clauses ? "," : ""
		}
		printf ");\n"
		leave()
}
function create_table(t, n, def, i) {
		enter("create_table", n, "table_name")
		assert_array(t["clauses"], rap("clauses", rap(n, "t")))
		parse_vals(t["clauses"], def, ",")
		parse_table(n, t["clauses"], t["field_of_clause"], t["field_names"], t["field_types"], t["field_defaults"], t["field_props"], t["clause_terms"], t["field_prop_set"])
		for (i in t) show(t[i], wrap(i, n))
		if (_print_["tables"])
				print_table_(n, t["clauses"], t["field_of_clause"], t["field_names"], t["field_types"], t["field_defaults"], t["field_props"], t["field_prop_set"])
		leave()
}

function print_type_list(t) { for ( t in the_types ) print t; }
function print_prop_list(p) { for ( p in the_props ) print p; }

function print_insert_(table_name, values, field_names, field_types, _, field_list, i) {
		enter("print_insert_", table_name, "table_name")
		show(values, "values")
		show(field_names, "field_names")
		show(field_types, "field_types")
		field_list = array_to_ident_list(field_names)
		show(field_list, "field list")
		printf "INSERT INTO %s%s VALUES(", safe_ident(table_name), field_list
		for ( i=1 ; i <= length(field_names) ; i++ )
				printf "%s%s", i==1 ? "" : ",",	pg_value(field_types[i], values[i])
		printf ");\n"
		leave()
}

function print_insert(t, n, values_csv, _, values) {
		enter("print_insert", n, "table_name")
		assert_array(t, "tables")
		values[""]=1; delete values[""]	# array values
		parse_vals(values, values_csv, ",")
		print_insert_(n, values, t[n]["field_names"], t[n]["field_types"])
		leave()
}

function tables_subarrays_init() {
		split("clauses field_of_clause field_names field_types field_defaults clause_terms field_props field_prop_set", _tables_subarrays_)
		show(_tables_subarrays, "_tables_subarrays_")
}

function globals_init() {
		_test_exit_code_ = 10
		_assert_exit_code_ = 11
		sq_keyword_init()
		pg_type_init()
		pg_prop_init()
		find_table_field_type_init()
		tables_subarrays_init()
		tables_init()
}

function run_tests_() {
		return \
				pair_tests() &&
				trim_tests() &&
				parse_vals_tests() &&
				parse_clause_tests() &&
				parse_field_terms_tests(_test_tables)
}

# Run tests silently and without exiting on failure.
# If any tests failed, rerun tests with verbose debugging
# and default exit behavior restored.
function run_tests(_, save_debug, save_test_exit_code) {
		enter("run_tests")
		_test_error_ = 0
		_num_tests_passed_ = 0
		save_debug = set_debug( 0 )
		save_test_exit_code = _test_exit_code_
		_test_exit_code_ = 0
		run_tests_()
		set_debug( save_debug )
		_test_exit_code_ = save_test_exit_code
		if ( _test_error_ ) {
				set_debug( 1 )
				_num_tests_passed_ = 0
				run_tests_()
		}
		leave(_num_tests_passed_, "number of tests passed")
		if (_test_error_) exit 1
}

function schematize_init() {
		if (schema) {
				_schema_name_ = gensub(/^\s*(\w+).*$/, "\\1", "1", schema)
				_schema_drop_ = schema ~ /!$/
				_schema_create_ = schema ~ /\?$/
				_schema_when_ = schema ~ /@$/ ? "TRANSACTION" : "BEGINNING"
		}
}

function schematize() {
		if (! _schema_name_) return
		enter("schematize", _schema_name_, "_schema_name_")
		if (_schema_drop_)
				printf "DROP SCHEMA IF EXISTS %s CASCADE;\n", _schema_name_
		if (_schema_create_ || _schema_drop_)
				printf "CREATE SCHEMA %s%s;\n", (_schema_drop_ ? "IF NOT EXISTS " : ""), _schema_name_
		printf "SET search_path TO %s;\n", _schema_name_
		leave()
}

function print_options_init(_, items, i) {
		enter("print_options_init", only_print, "only_print")
		if (! only_print ) only_print="input,tables,inserts,indices,transactions"
		split(only_print, items, /\s*,\s*/)
		for (i in items) _print_[items[i]] = 1
		show(_print_, "_print_")
		leave()
}

BEGIN {
		set_debug( 0 )
		globals_init()
		run_tests()
		set_debug( verbose )
    print_options_init()
		schematize_init()
		if (_schema_when_ == "BEGINNING") schematize()
}
_print_["input"] {printf "%s%s\n", $1 ~ /^--/ ? "" : "-- ", $0; }
$1 == "CREATE" && $2 == "TABLE" {
		enter("CREATE TABLE")
		define_subarray(tables,$3, "tables")
		define_subarrays(tables[$3], _tables_subarrays_, rap($3, "tables"))
		create_table( tables[$3], $3, gensub(/^[^()]*\((.*)\);$/, "\\1", "1") )
		leave()
		next
}
# _insert_parts_[1] = table name
# _insert_parts_[2] = comma-separated values to insert
_print_["inserts"] && match($0, /^\s*INSERT\s+INTO\s+"?([^[:space:]"]+)"?\s+VALUES\s*\((.*\S)\s*\)\s*;\s*$/, _insert_parts_) {
		enter("INSERT INTO", _insert_parts_, "_insert_parts_")
		print_insert( tables,	_insert_parts_[1],	_insert_parts_[2] )
		leave()
		next
}
# _index_parts_[1] = CREATE (UNIQUE)? INDEX
# _index_parts_[2] = (UNIQUE)?
# _index_parts_[3] = index-name
# _index_parts_[4] = table-name
# _index_parts_[5] = comma-separated-fields
_print_["indices"] && match($0, /^\s*(CREATE\s+(UNIQUE\s+)?INDEX)\s+(\w+)\s+ON\s+(\w+)\s*\(\s*([^()]*\w)\s*\)\s*;\s*$/, _index_parts_) {
		enter("CREATE INDEX", _index_parts_, "_index_parts_")
		split(_index_parts_[5], _index_fields_, /\s*,\s*/)
		show(_index_fields_, "_index_fields_")
		printf "%s %s ON %s %s;\n",
				_index_parts_[1],
				safe_ident(_index_parts_[3]),
				safe_ident(_index_parts_[4]),
				array_to_ident_list(_index_fields_)
		leave()
		next
}
_print_["transactions"] && /^BEGIN TRANSACTION;$/ {
		print
		if (_schema_when_ == "TRANSACTION") schematize()
		next
}
_print_["transactions"] && /^COMMIT;$/ { print; next }
END {
		if (_print_["types"]) print_type_list()
		if (_print_["props"]) print_prop_list()
}
