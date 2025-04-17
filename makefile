OBJ_DIR = obj

compiler: $(OBJ_DIR)/Lexer.o $(OBJ_DIR)/main.o $(OBJ_DIR)/Parser.o $(OBJ_DIR)/Utilities.o
	g++ -o gsc $^

obj/%.o: source/%.cpp | $(OBJ_DIR)
	g++ -c -o $@ $< -std=c++20 -fno-rtti

$(OBJ_DIR):
	mkdir -p $@
	clear