#Question 19 : Makefile

OCAML=ocamlopt
SRC=src
BIN=bin
EXEC=square
EXEC3=hexa
EXEC2=play
OCAMLFLAGS=-c
LIBS=graphics.cmxa
SRC_SQUARE=$(SRC)/common.ml $(SRC)/labysquare.ml  $(SRC)/display.ml  $(SRC)/projet.ml
SRC_PLAY=$(SRC)/common.ml $(SRC)/labysquare.ml $(SRC)/display.ml $(SRC)/display_user.ml  $(SRC)/interactive.ml
SRC_HEXA=$(SRC)/common.ml $(SRC)/labyhexa.ml $(SRC)/display_hexa.ml $(SRC)/projet_hexa.ml
all:
	$(OCAML) -I $(SRC) -o $(BIN)/$(EXEC) $(LIBS) $(SRC_SQUARE)
	$(OCAML) -I $(SRC) -o $(BIN)/$(EXEC2) $(LIBS) $(SRC_PLAY)
	$(OCAML) -I $(SRC) -o $(BIN)/$(EXEC3) $(LIBS) $(SRC_HEXA)
	rm -rf  $(SRC)/*.cm* $(SRC)/*.o $(SRC)/*~

square:
	$(OCAML) -I $(SRC) -o $(BIN)/$(EXEC) $(LIBS) $(SRC_SQUARE)
	rm -rf  $(SRC)/*.cm* $(SRC)/*.o $(SRC)/*~

play:
	$(OCAML) -I $(SRC) -o $(BIN)/$(EXEC2) $(LIBS) $(SRC_SQUARE)
	rm -rf  $(SRC)/*.cm* $(SRC)/*.o $(SRC)/*~

hexa:
	$(OCAML) -I $(SRC) -o $(BIN)/$(EXEC3) $(LIBS) $(SRC_HEXA)
	rm -rf  $(SRC)/*.cm* $(SRC)/*.o $(SRC)/*~
clean:
	rm -rf $(BIN)/$(EXEC) $(BIN)/$(EXEC2) $(BIN)/$(EXEC3) $(SRC)/*.cm* $(SRC)/*.o  $(SRC)/*~
