OBJ_DIR = obj

gsc: $(OBJ_DIR)/Lexer.o $(OBJ_DIR)/main.o $(OBJ_DIR)/Parser.o $(OBJ_DIR)/Utilities.o
	g++ -o gsc $^
	clear

run: gsc
	./gsc code.cast

obj/%.o: source/%.cpp | $(OBJ_DIR)
	g++ -c -o $@ $< -std=c++20 -fno-rtti

$(OBJ_DIR):
	mkdir -p $@