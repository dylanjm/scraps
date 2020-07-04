NAME=scraps
DEST=$(XDG_BIN_HOME)
BINARY=$(DEST)/$(NAME)
SCRIPT=$(PWD)/$(NAME)
CL=cl-launch
LISP=sbcl

.PHONY: all $(NAME) test clean

all: $(NAME) test

$(NAME):
	@$(CL) --output $(NAME) \
	--dump ! \
	--lisp sbcl \
	--quicklisp \
	--dispatch-system $(NAME)/src/main \
	--system $(NAME)

test: $(NAME)
	@$(LISP) --noinform \
	--non-interactive \
	--load scraps.asd \
	--eval '(ql:quickload :scraps :silent t)' \
	--eval '(ql:quickload :scraps/test :silent t)' \
	--eval '(prove:run #P"tests/test-memes.lisp")'

install: $(NAME)
	@ln -sf $(SCRIPT) $(BINARY)
	@$(SCRIPT) create-symlinks $(NAME)

clean:
	@rm -f $(NAME)
