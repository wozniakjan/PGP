PROJ = raytracer
OBJ  = $(shell ls src/*.hs)
BIN  = bin/
SRC  = src/

.phony: clean edit $(BIN) run

all: $(BIN)$(PROJ)

$(BIN)$(PROJ): $(OBJ) $(BIN)
	ghc --make $(OBJ) -o $(BIN)$(PROJ)


$(BIN): 
	$(shell mkdir $(BIN))

clean:
	rm -rf $(SRC)*.hi $(SRC)*.o $(BIN)$(PROJ)

edit:
	vim -S ses.vim
