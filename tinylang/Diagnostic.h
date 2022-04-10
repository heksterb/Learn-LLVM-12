#pragma once

#include <utility>

#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/SMLoc.h>


class Diagnostics {
public:
	enum {
#define DIAG(id, level, message) id,
#include "Diagnostic.def"
		};

protected:
	static const char *getMessage(unsigned id);
	static llvm::SourceMgr::DiagKind getKind(unsigned id);
	
	llvm::SourceMgr	&fSourceManager;
	unsigned	fNumErrors;

public:
			Diagnostics(llvm::SourceMgr &sourceManager) :
				fSourceManager(sourceManager),
				fNumErrors(0)
				{}
	
	unsigned	numErrors() const { return fNumErrors; }
	
	template <typename ...Args>
	void		report(llvm::SMLoc location, unsigned id, Args &&...arguments);
	};


template <typename... Args>
void Diagnostics::report(
	llvm::SMLoc	location,
	unsigned	id,
	Args		&&...arguments
	)
{
std::string message = llvm::formatv(
	getMessage(id), std::forward<Args>(arguments)...
	).str();
llvm::SourceMgr::DiagKind kind = getKind(id);
fSourceManager.PrintMessage(location, kind, message);
if (kind == llvm::SourceMgr::DK_Error) fNumErrors++;
}

