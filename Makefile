# Need some GNU Awk 4.1 features
awk = /usr/local/bin/gawk
BASHer = sqlite-to-pgsql
AWKer = sqlite-to-pgsql.awk
TOOLS = $(BASHer) $(AWKer)
tests: $(AWKer)
	$(awk) -v verbose=1 -f ./sqlite-to-pgsql.awk /dev/null 2>&1 | tee tests.debug
tests.pgsql: tests.sqlite $(AWKer) tests.good.pgsql
	{ $(awk) -v schema='tests!' -f ./sqlite-to-pgsql.awk tests.sqlite 2>tests.debug | tee tests.pgsql; } && diff tests.pgsql tests.good.pgsql
places.pgsql: $(TOOLS)
	sqlite-to-pgsql -schema + places.sqlite places.pgsql 2>places.debug
