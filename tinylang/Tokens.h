#pragma once

#include <initializer_list>

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>


struct Token {
public:
	enum Kind {
#define TOKEN(id) id,
#include "Tokens.def"
		};

protected:
	Kind		fKind;
	const char	*fText;
	size_t		fLength;

public:
	static const char *getTokenName(Token::Kind);
	static const char *getPunctuatorSpelling(Token::Kind);
	static const char *getKeywordSpelling(Token::Kind);
	
	
			Token() : fKind(kUnknown) {}
			Token(Kind, const char *text, size_t length);
	
	Kind		kind() const { return fKind; }
	size_t		length() const { return fLength; }
	llvm::SMLoc	location() const { return llvm::SMLoc::getFromPointer(fText); }
	llvm::StringRef	identifier() const;
	llvm::StringRef	literal() const;
	
	bool		isOneOf(std::initializer_list<Kind>) const;
	};
