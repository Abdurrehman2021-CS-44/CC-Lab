#include <iostream>
#include <vector>
#include <string>
#include <cctype>
#include <map>
#include <fstream>
using namespace std;

enum TokenType {
    T_INT, T_ID, T_NUM, T_FLOAT, T_STRING, T_IF, T_ELSE, T_RETURN,
    T_ASSIGN, T_PLUS, T_MINUS, T_MUL, T_DIV,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,
    T_SEMICOLON, T_GT, T_LT, T_EOF,
    T_FOR, T_WHILE, T_EQ, T_LE, T_AND
};

struct Token {
    TokenType type;
    string value;
    int lineNum;
    Token(TokenType tt, string v) {
        type = tt;
        value = v;
    }
};

string tokenTypeToString(TokenType type) {
    switch(type) {
        case T_INT: return "int";
        case T_ID: return "identifier";
        case T_NUM: return "number";
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
        default: return "Unknown Token";
    }
}

class Lexer {
private:
    string src;
    size_t pos;
    int lineNum;

public:
    Lexer(const string &src) {
        this->src = src;
        this->pos = 0;
        this->lineNum = 0;
    }

    string consumeNumber() {
        size_t start = pos;
        while (pos < src.size() && isdigit(src[pos])) {
            pos++;
        }
        return src.substr(start, pos - start);
    }

    string consumeWord() {
        size_t start = pos;
        while (pos < src.size() && (isalnum(src[pos]) || src[pos] == '_')) {
            pos++;
        }
        return src.substr(start, pos - start);
    }

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (pos < src.size()) {
            char current = src[pos];

            if (isspace(current)) {
                if (current == '\n') {
                    lineNum++;
                } else {
                }
                pos++;
                continue;
            }

            if (isdigit(current)) {
                tokens.push_back(Token(T_NUM, consumeNumber()));
                tokens.back().lineNum = lineNum;
                continue;
            }

            if (isalpha(current)) {
                string word = consumeWord();
                TokenType type = T_ID;
                if (word == "int") type = T_INT;
                else if (word == "if") type = T_IF;
                else if (word == "else") type = T_ELSE;
                else if (word == "return") type = T_RETURN;
                
                tokens.push_back(Token(type, word));
                tokens.back().lineNum = lineNum;
                continue;
            }

            Token token(T_EOF, "");
            switch (current) {
                case '=':
                    if (src[pos + 1] == '=') {
                        token = Token(T_EQ, "==");
                        pos++;
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
                    if (src[pos + 1] == '&') {
                        token = Token(T_AND, "&&");
                        pos++;
                    } else {
                        cout << "Unexpected character: " << current << " at line " << lineNum << endl;
                        exit(1);
                    }
                    break;
                default:
                    cout << "Unexpected character: " << current << " at line " << lineNum << endl;
                    exit(1);
            }
            
            token.lineNum = lineNum;
            tokens.push_back(token);

            pos++;
        }

        Token eofToken(T_EOF, "");
        eofToken.lineNum = lineNum;
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
        if (tokens[pos].type == T_INT) {
            parseDeclaration();
        } else if (tokens[pos].type == T_ID) {
            parseAssignment();
        } else if (tokens[pos].type == T_IF) {
            parseIfStatement();
        } else if (tokens[pos].type == T_RETURN) {
            parseReturnStatement();
        } else if (tokens[pos].type == T_LBRACE) {
            parseBlock();
        } else {
            cout << "Syntax error: unexpected token " << tokens[pos].value << endl;
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

    void parseDeclaration() {
        expect(T_INT);
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
        if (tokens[pos].type == T_GT) {
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
        if (tokens[pos].type == T_NUM || tokens[pos].type == T_ID) {
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
    }

    string line;
    string code;

    while (getline(file, line)) {
        code += line + "\n";
    }

    file.close();

    Lexer lexer(code);
    vector<Token> tokens = lexer.tokenize();

    Parser parser(tokens);
    parser.parseProgram();

    return 0;
}