#pragma once

#include "Diagnostic.h"
#include "Tokens.h"


struct Lexer {
protected:
	static bool	isIdentifierHead(char),
			isIdentifierBody(char),
			isVerticalWhitespace(char);
	
	
	Diagnostics	&fDiagnostics;
	
	const char	*fBufferStart,
			*fBuffer;
	
	Token		identifier(),
			number(),
			string();
	bool		space(),
			comment();

public:
			Lexer(
				Diagnostics &diagnostics,
				llvm::StringRef buffer
				) :
				fDiagnostics(diagnostics),
				fBufferStart(buffer.begin()),
				fBuffer(fBufferStart)
				{}
	
	Token		next();
	};
