#include <iostream>
#include <vector>
#include <string>
#include <cctype>
#include <map>
#include <fstream>
using namespace std;

enum TokenType {
    T_INT, T_FLOAT, T_DOUBLE, T_STRING, T_BOOL, T_CHAR,
    T_ID, T_NUM, T_NUM_FLOAT, T_IF, T_ELSE, T_RETURN,
    T_ASSIGN, T_PLUS, T_MINUS, T_MUL, T_DIV,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,
    T_SEMICOLON, T_GT, T_LT, T_EOF,
    T_FOR, T_WHILE, T_EQ, T_LE, T_AND
};

struct Token {
    TokenType type;
    string value;
    int lineNum;
    int colNum;
    Token(TokenType tt, string v) {
        type = tt;
        value = v;
    }
};

string tokenTypeToString(TokenType type) {
    switch(type) {
        case T_INT: return "int";
        case T_FLOAT: return "float";
        case T_DOUBLE: return "double";
        case T_STRING: return "string";
        case T_BOOL: return "bool";
        case T_CHAR: return "char";
        case T_ID: return "identifier";
        case T_NUM: return "number";
        case T_NUM_FLOAT: return "float number";
        case T_ASSIGN: return "assignment (=)";
        case T_PLUS: return "plus (+)";
        case T_MINUS: return "minus (-)";
        case T_MUL: return "multiply (*)";
        case T_DIV: return "divide (/)";
        case T_LPAREN: return "left parenthesis (()";
        case T_RPAREN: return "right parenthesis ())";
        case T_LBRACE: return "left brace ({)";
        case T_RBRACE: return "right brace (})";
        case T_SEMICOLON: return "semicolon (;)";
        case T_GT: return "greater than (>)";
        case T_LT: return "less than (<)";
        case T_IF: return "if";
        case T_ELSE: return "else";
        case T_RETURN: return "return";
        case T_EOF: return "end of file";
        case T_WHILE: return "while";
        case T_EQ: return "equals (==)";
        default: return "Unknown Token";
    }
}

class Lexer {
private:
    string src;
    size_t pos;
    int lineNum;
    int colNum;

    void skipComment() {
        pos += 2;
        colNum += 2;
        
        while (pos < src.size() && src[pos] != '\n') {
            pos++;
            colNum++;
        }
    }

public:
    Lexer(const string &src) {
        this->src = src;
        this->pos = 0;
        this->lineNum = 1;
        this->colNum = 1;
    }

    string consumeNumber() {
        size_t start = pos;
        while (pos < src.size() && isdigit(src[pos])) {
            pos++;
            colNum++;
        }
        return src.substr(start, pos - start);
    }

    string consumeFloatNumber() {
        size_t start = pos;
        while (pos < src.size() && (isdigit(src[pos]) || src[pos] == '.')) {
            pos++;
            colNum++;
        }
        return src.substr(start, pos - start);
    }

    string consumeWord() {
        size_t start = pos;
        while (pos < src.size() && (isalnum(src[pos]) || src[pos] == '_')) {
            pos++;
            colNum++;
        }
        return src.substr(start, pos - start);
    }

    string consumeCharacter() {
        size_t start = pos;
        pos++; // skip opening quote
        colNum++;
        while (pos < src.size() && src[pos] != '\'') {
            pos++;
            colNum++;
        }
        if (pos < src.size()) {
            pos++; // skip closing quote
            colNum++;
        }
        return src.substr(start + 1, pos - start - 2);
    }

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (pos < src.size()) {
            char current = src[pos];

            // Handle comments
            if (current == '/' && pos + 1 < src.size() && src[pos + 1] == '/') {
                skipComment();
                continue;
            }

            if (isspace(current)) {
                if (current == '\n') {
                    lineNum++;
                    colNum = 1;
                } else {
                    colNum++;
                }
                pos++;
                continue;
            }

            if (isdigit(current)) {
                if (pos + 1 < src.size() && src[pos + 1] == '.') {
                    tokens.push_back(Token(T_NUM_FLOAT, consumeFloatNumber()));
                } else {
                    tokens.push_back(Token(T_NUM, consumeNumber()));
                }
                tokens.back().lineNum = lineNum;
                tokens.back().colNum = colNum;
                continue;
            }

            if (isalpha(current)) {
                string word = consumeWord();
                TokenType type = T_ID;
                if (word == "int") type = T_INT;
                else if (word == "float") type = T_FLOAT;
                else if (word == "double") type = T_DOUBLE;
                else if (word == "string") type = T_STRING;
                else if (word == "bool") type = T_BOOL;
                else if (word == "char") type = T_CHAR;
                else if (word == "if") type = T_IF;
                else if (word == "else") type = T_ELSE;
                else if (word == "return") type = T_RETURN;
                else if (word == "while") type = T_WHILE;

                tokens.push_back(Token(type, word));
                tokens.back().lineNum = lineNum;
                tokens.back().colNum = colNum;
                continue;
            }

            if (current == '\'') {
                tokens.push_back(Token(T_CHAR, consumeCharacter()));
                tokens.back().lineNum = lineNum;
                tokens.back().colNum = colNum;
                continue;
            }

            Token token(T_EOF, "");
            switch (current) {
                case '=':
                    if (pos + 1 < src.size() && src[pos + 1] == '=') {
                        token = Token(T_EQ, "==");
                        pos++;
                        colNum++;
                    } else {
                        token = Token(T_ASSIGN, "=");
                    }
                    break;
                case '+': token = Token(T_PLUS, "+"); break;
                case '-': token = Token(T_MINUS, "-"); break;
                case '*': token = Token(T_MUL, "*"); break;
                case '/': token = Token(T_DIV, "/"); break;
                case '(': token = Token(T_LPAREN, "("); break;
                case ')': token = Token(T_RPAREN, ")"); break;
                case '{': token = Token(T_LBRACE, "{"); break;
                case '}': token = Token(T_RBRACE, "}"); break;
                case ';': token = Token(T_SEMICOLON, ";"); break;
                case '>': token = Token(T_GT, ">"); break;
                case '<': token = Token(T_LT, "<"); break;
                case '&':
                    if (pos + 1 < src.size() && src[pos + 1] == '&') {
                        token = Token(T_AND, "&&");
                        pos++;
                        colNum++;
                    } else {
                        cout << "Unexpected character: " << current << " at line " << lineNum << ", col " << colNum << endl;
                        exit(1);
                    }
                    break;
                default:
                    cout << "Unexpected character: " << current << " at line " << lineNum << ", col " << colNum << endl;
                    exit(1);
            }
            
            token.lineNum = lineNum;
            token.colNum = colNum;
            tokens.push_back(token);

            pos++;
            colNum++;
        }

        Token eofToken(T_EOF, "");
        eofToken.lineNum = lineNum;
        eofToken.colNum = colNum;
        tokens.push_back(eofToken);
        return tokens;
    }
};

class Parser {
private:
    vector<Token> tokens;
    size_t pos;

public:
    Parser(const vector<Token> &tokens) {
        this->tokens = tokens;
        this->pos = 0;
    }

    void parseProgram() {
        while (tokens[pos].type != T_EOF) {
            parseStatement();
        }
        cout << "Parsing completed successfully! No Syntax Error" << endl;
    }

    void parseStatement() {
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT ||
            tokens[pos].type == T_DOUBLE || tokens[pos].type == T_STRING ||
            tokens[pos].type == T_BOOL || tokens[pos].type == T_CHAR) {
            parseDeclaration();
        } else if (tokens[pos].type == T_ID) {
            parseAssignment();
        } else if (tokens[pos].type == T_IF) {
            parseIfStatement();
        } else if (tokens[pos].type == T_WHILE) {
            parseWhileStatement();
        } else if (tokens[pos].type == T_RETURN) {
            parseReturnStatement();
        } else if (tokens[pos].type == T_LBRACE) {
            parseBlock();
        } else {
            cout << "Syntax error: unexpected token " << tokens[pos].value 
                 << " at line " << tokens[pos].lineNum << endl;
            exit(1);
        }
    }

    void parseBlock() {
        expect(T_LBRACE);
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF) {
            parseStatement();
        }
        expect(T_RBRACE);
    }

    void parseWhileStatement() {
        expect(T_WHILE);
        expect(T_LPAREN);
        parseExpression();
        expect(T_RPAREN);
        parseStatement();
    }

    void parseDeclaration() {
        expect(tokens[pos].type); // Expect type (int, float, double, etc.)
        expect(T_ID);
        
        if (tokens[pos].type == T_ASSIGN) {
            expect(T_ASSIGN);
            parseExpression();
        }
        
        expect(T_SEMICOLON);
    }

    void parseAssignment() {
        expect(T_ID);
        expect(T_ASSIGN);
        parseExpression();
        expect(T_SEMICOLON);
    }

    void parseIfStatement() {
        expect(T_IF);
        expect(T_LPAREN);
        parseExpression();
        expect(T_RPAREN);
        parseStatement();
        if (tokens[pos].type == T_ELSE) {
            expect(T_ELSE);
            parseStatement();
        }
    }

    void parseReturnStatement() {
        expect(T_RETURN);
        parseExpression();
        expect(T_SEMICOLON);
    }

    void parseExpression() {
        parseTerm();
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS) {
            pos++;
            parseTerm();
        }
        if (tokens[pos].type == T_GT || tokens[pos].type == T_EQ) {
            pos++;
            parseExpression();
        }
    }

    void parseTerm() {
        parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV) {
            pos++;
            parseFactor();
        }
    }

    void parseFactor() {
        if (tokens[pos].type == T_NUM || tokens[pos].type == T_NUM_FLOAT ||
            tokens[pos].type == T_ID || tokens[pos].type == T_CHAR) {
            pos++;
        } else if (tokens[pos].type == T_LPAREN) {
            expect(T_LPAREN);
            parseExpression();
            expect(T_RPAREN);
        } else {
            cout << "Syntax error: unexpected token " << tokens[pos].value << endl;
            exit(1);
        }
    }

    void expect(TokenType type) {
        if (tokens[pos].type == type) {
            pos++;
        } else {
            cout << "Syntax error: expected " << tokenTypeToString(type)
                 << " but found '" << tokens[pos].value 
                 << "' at line " << tokens[pos].lineNum << endl;
            exit(1);
        }
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        cerr << "Provide the filename." << endl;
        return 1;
    }
    
    string filename = argv[1];

    ifstream file(filename);

    if (!file.is_open()) {
        cout << filename << " does not exist." << endl;
        return 1;
    }

    string line;
    string code;

    while (getline(file, line)) {
        code += line + "\n";
    }

    cout << "Input Code:" << endl;
    cout << code << endl;

    file.close();

    Lexer lexer(code);
    vector<Token> tokens = lexer.tokenize();

    Parser parser(tokens);
    parser.parseProgram();

    return 0;
}