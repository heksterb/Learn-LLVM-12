#include <ctype.h>

#include <algorithm>

#include "Lexer.h"


bool Lexer::isIdentifierHead(
	const char	c
	)
{
return isalpha(c) || c == '_';
}


bool Lexer::isIdentifierBody(
	const char	c
	)
{
return isalnum(c) || c == '_';
}


bool Lexer::isVerticalWhitespace(
	const char	c
	)
{
return c == '\r' || c == '\n';
}


static const struct KeywordMap {
	Token::Kind	kind;
	const char	*name;
	} gKeywords[] = {
#define KEYWORD(name, flag) { Token::keyword ## name, #name },
#include "Tokens.def"
	};


/*	identifier

*/
Token Lexer::identifier()
{
// mark beginning of text
const char *const start = fBuffer;

// scan to the end of the identifier
while (isIdentifierBody(*++fBuffer));

const size_t length = fBuffer - start;
const KeywordMap *const keyword = std::find_if(
	std::begin(gKeywords), std::end(gKeywords),
	[start, length](const auto &keyword) {
		return strncmp(start, keyword.name, length) == 0;
		}
	);

const Token::Kind kind =
	keyword != std::end(gKeywords) ?
		keyword->kind :
		Token::kIdentifier;

return Token(kind, start, fBuffer - start);
}


/*	number

*/
Token Lexer::number()
{
// mark beginning of text
const char *const start = fBuffer;

// scan to the end of the number
bool hasHexadecimalCharacters = false;
while (*++fBuffer)
	// not a hexadecimal (and, by implication, not a decimal) digit?
	if (!isxdigit(*fBuffer))
		break;
	
	// not a decimal (and, by implication, therefore a hexadecimal) digit?
	else if (!isdigit(*fBuffer))
		hasHexadecimalCharacters = true;

// number radix?
switch (*fBuffer) {
	// hexadecimal?
	case 'H':
		++fBuffer;
		break;
	
	// decimal?
	default:
		// no hexadecimal radix, but has hexadecimal characters?
		if (hasHexadecimalCharacters)
			fDiagnostics.report(llvm::SMLoc::getFromPointer(fBuffer), Diagnostics::errorHexadecimalDigitInDecimal);
	}

/* Note that at this point, the information that the token is hexadecimal is lost again;
   but it can be deduced from the last character of the literal text. */
return Token(Token::kIntegerLiteral, start, fBuffer - start);
}


/*	string

*/
Token Lexer::string()
{
// mark beginning of text
const char *const start = fBuffer;

// scan until end of string
while (*fBuffer && *fBuffer != *start && !isVerticalWhitespace(*fBuffer)) fBuffer++;

if (isVerticalWhitespace(*fBuffer))
	fDiagnostics.report(llvm::SMLoc::getFromPointer(fBuffer), Diagnostics::errorUnterminatedCharaterOrString);

return Token(Token::kStringLiteral, start, fBuffer - start);
}


/*	space

*/
bool Lexer::space()
{
const char *const start = fBuffer;

while (isspace(*fBuffer)) fBuffer++;

return fBuffer > start;
}


/*	comment
	
*/
bool Lexer::comment()
{
const char *const start = fBuffer;
unsigned level = 0;

for (; *fBuffer; fBuffer++)
	if (fBuffer[0] == '(' && fBuffer[1] == '*')
		fBuffer++,
		level++;
	
	else if (level == 0)
		break;
	
	else if (fBuffer[0] == '*' && fBuffer[1] == ')')
		fBuffer++,
		level--;

if (level > 0)
	fDiagnostics.report(llvm::SMLoc::getFromPointer(fBuffer), Diagnostics::errorUnterminatedComment);

return fBuffer > start;
}


/*	next
	Scan and obtain the next token
*/
Token Lexer::next()
{
// skip over spaces and comments
while (space() || comment());

// end of input?
if (!*fBuffer)
	return Token(Token::kEOF, "", 0);

// first character of identifier?
else if (isIdentifierHead(*fBuffer))
	return identifier();

// number?
else if (isdigit(*fBuffer))
	return number();

// string?
else if (*fBuffer == '"' || *fBuffer == '\'')
	return string();

// single-character token
else {
	/* So this should come out of macro expansion from Tokens.def; but the preprocessor
	   clearly can'*/
	static const struct Map {
		char		c;
		Token::Kind	k;
		} map[] = {
		{ '=', Token::kEqual },
		{ '#', Token::kHash },
		{ '+', Token::kPlus },
		{ '-', Token::kMinus },
		{ '*', Token::kAsterisk },
		{ '/', Token::kSlash },
		{ ',', Token::kComma },
		{ '.', Token::kPeriod },
		{ ':', Token::kColon },
		{ ';', Token::kSemicolon },
		{ '(', Token::kParenthesisLeft },
		{ ')', Token::kParenthesisRight },
		{ '<', Token::kLessThan },
		{ '>', Token::kGreaterThan }
		};
	
	const char *const start = fBuffer;
	
	// find kind of token
	Token::Kind kind;
	if (
		const Map *const m = std::find_if(
			std::begin(map), std::end(map),
			[c = *fBuffer++](const Map &m) { return m.c == c; }
			);
		m != std::end(map)
		)
		kind = m->k;
	
	else
		kind = Token::kUnknown;
	
	// two-character tokens
	if (kind == Token::kLessThan && *fBuffer == '=') {
		kind = Token::kLessThanEqual;
		fBuffer++;
		}
	
	else if (kind == Token::kGreaterThan && *fBuffer == '=') {
		kind = Token::kGreaterThanEqual;
		fBuffer++;
		}
	
	else if (kind == Token::kColon && *fBuffer == '=') {
		kind = Token::kColonEqual;
		fBuffer++;
		}
	
	return Token(kind, start, fBuffer - start);
	}
}
