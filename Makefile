.PHONY: contact-nodes run

contact-nodes:
	lfe -sname contact1@localhost -detached
	lfe -sname contact2@localhost -detached

run:
	lfe -sname  mynode@localhost \
	    -pa     ./simple_cache/ebin \
	    -pa     ./resource_discovery/ebin \
	    -boot   ./simple_cache \
	    -config sys
