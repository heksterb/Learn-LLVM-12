#include <algorithm>

#include <llvm/Support/ErrorHandling.h>

#include "Tokens.h"


static const char *const gTokenNames[] = {
#define TOKEN(id) #id,
#define KEYWORD(id, flag) #id,
#include "Tokens.def"
// implicily undefs TOKEN, KEYWORD
	nullptr
	};


const char *Token::getTokenName(
	Token::Kind	kind
	)
{
return gTokenNames[kind];
}


const char *Token::getPunctuatorSpelling(
	Token::Kind	kind
	)
{
const char *result;

switch (kind) {
#define PUNCTUATOR(id, spelling) case Token::id: result = spelling;
#include "Tokens.def"
// implicily undefs PUNCTUATOR
	// default case covers non-punctuator token kinds
	default:	result = nullptr; break;
	}

return result;
}


const char *Token::getKeywordSpelling(
	Token::Kind	kind
	)
{
const char *result;

switch (kind) {
#define KEYWORD(id, flag) case Token::keyword ## id: result = #id;
#include "Tokens.def"
// implicitly undefs KEYWORD
	// default case covers non-keyword token kinds
	default:	result = nullptr; break;
	}

return result;
}


Token::Token(
	Kind		kind,
	const char	*text,
	size_t		length
	) :
	fKind(kind),
	fText(text),
	fLength(length)
{
}


/*	identifier
	Return the text of the token, while asserting that it is an identifier
*/
llvm::StringRef	Token::identifier() const
{
if (fKind != kIdentifier) throw "can't get identifier of non-identifier";

return llvm::StringRef(fText, fLength);
}


/*	literal
	Return the text of the token, while asserting that it is some literal
*/
llvm::StringRef	Token::literal() const
{
if (!isOneOf({ kIntegerLiteral, kStringLiteral })) throw "can't get literal text of non-literal";

return llvm::StringRef(fText, fLength);
}


/*	isOneOf
	Return whether this token is of one of the given kinds
*/
bool Token::isOneOf(
	std::initializer_list<Kind> kinds
	) const
{
// return if our kind is any of the given kinds
return std::any_of(
	kinds.begin(), kinds.end(),
	[kind = fKind](const Kind k) { return k == kind; }
	);
}
