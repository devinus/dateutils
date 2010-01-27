ERL	?= erl

all:
	@mkdir -p ebin
	@$(ERL) -noinput \
		-eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

clean:
	@rm -fv ebin/*.beam
