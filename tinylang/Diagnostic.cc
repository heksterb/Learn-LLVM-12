#include "Diagnostic.h"


static const char *const gMessages[] = {
#define DIAG(id, level, message) message,
#include "Diagnostic.def"
// implicily undefs DIAG
	};


static const llvm::SourceMgr::DiagKind gKinds[] = {
#define DIAG(id, level, message) llvm::SourceMgr::DK_##level,
#include "Diagnostic.def"
// implicily undefs DIAG
	};


const char *Diagnostics::getMessage(
	unsigned	id
	)
{
return gMessages[id];
}


llvm::SourceMgr::DiagKind Diagnostics::getKind(
	unsigned	id
	)
{
return gKinds[id];
}
