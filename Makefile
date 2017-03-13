#---------------------------------start------------------------------
VERSION=0.1.0 

.PHONY: all compile deps clean cleanall eunit doc   

all: cleanall deps compile

help:       
	@echo
	@echo "Usage: "
	@echo "       ./make {compile|clean|cleanall|eunit|ct|doc|dialyzer}" 
	@echo
	@echo
	@echo                                       

compile: deps                                       
	./rebar3 compile

deps:
	./rebar3 deps                            

clean:
	./rebar3 clean 

cleanall:
	./rebar3 clean -a

ct:                                              
	./rebar3 ct --dir=test -c -v 
	./rebar3 cover

eunit:
	./rebar3 eunit -c -v 

doc :
	@./rebar3 edoc skip_deps=true                 

dialyzer: compile                        
	./rebar3 dialyzer
	@dialyzer -r apps --src 

#-------------------------end--------------------------------------
