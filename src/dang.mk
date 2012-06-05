
GHCFLAGS   := -Wall -isrc -hidir src -odir src
ALEXFLAGS  := -g
HAPPYFLAGS := -g -i


dang_sources := \
	src/CodeGen.hs \
	src/Colors.hs \
	src/Compile.hs \
	src/Compile/LambdaLift.hs \
	src/Compile/Rename.hs \
	src/Core/AST.hs \
	src/Core/Interface.hs \
	src/Dang/FileName.hs \
	src/Dang/IO.hs \
	src/Dang/Monad.hs \
	src/Dang/Tool.hs \
	src/Data/ClashMap.hs \
	src/Link.hs \
	src/Main.hs \
	src/ModuleSystem.hs \
	src/ModuleSystem/Export.hs \
	src/ModuleSystem/Imports.hs \
	src/ModuleSystem/Interface.hs \
	src/ModuleSystem/Resolve.hs \
	src/ModuleSystem/ScopeCheck.hs \
	src/ModuleSystem/Types.hs \
	src/Pretty.hs \
	src/Prim.hs \
	src/QualName.hs \
	src/ReadWrite.hs \
	src/Syntax.hs \
	src/Syntax/AST.hs \
	src/Syntax/Layout.hs \
	src/Syntax/Lexer.hs \
	src/Syntax/Lexeme.hs \
	src/Syntax/Parser.hs \
	src/Syntax/ParserCore.hs \
	src/Syntax/Quote.hs \
	src/Syntax/Renumber.hs \
	src/Traversal.hs \
	src/TypeChecker.hs \
	src/TypeChecker/CheckKinds.hs \
	src/TypeChecker/CheckTypes.hs \
	src/TypeChecker/Env.hs \
	src/TypeChecker/Monad.hs \
	src/TypeChecker/Quote.hs \
	src/TypeChecker/Subsumption.hs \
	src/TypeChecker/Types.hs \
	src/TypeChecker/Unify.hs \
	src/Utils.hs \
	src/Variables.hs

dang_objects    := $(dang_sources:.hs=.o)
dang_interfaces := $(dang_sources:.hs=.hi)

dang_packages := $(addprefix -package ,\
	array base bytestring cereal containers directory filepath GraphSCC \
	llvm-pretty monadLib pretty process syb template-haskell text)

build/bin/dang: GHCFLAGS   += -hide-all-packages $(dang_packages)
build/bin/dang: LDFLAGS    := -Wall -hide-all-packages $(dang_packages)
build/bin/dang: OBJECTS    := $(dang_objects)
build/bin/dang: $(dang_objects) | build/bin
	$(call cmd,link_hs)

-include src/.depend.dang

src/.depend.dang: SOURCES := $(dang_sources)
src/.depend.dang: $(dang_sources)
	$(call cmd,hs_depend)

all: build/bin/dang

clean::
	$Q$(RM) $(dang_objects) $(dang_interfaces)

mrproper::
	$Q$(RM) src/Syntax/Parser.hs src/Syntax/Lexer.hs
	$Q$(RM) src/.depend.dang


# Testing ######################################################################

test_sources := \
	src/Colors.hs \
	src/Core/AST.hs \
	src/Dang/IO.hs \
	src/Dang/Monad.hs \
	src/ModuleSystem/Export.hs \
	src/Pretty.hs \
	src/QualName.hs \
	src/Syntax/AST.hs \
	src/Tests.hs \
	src/Tests/Monadic.hs \
	src/Tests/QualName.hs \
	src/Tests/Subsumption.hs \
	src/Tests/Types.hs \
	src/Tests/Unification.hs \
	src/Tests/Utils.hs \
	src/TypeChecker/Monad.hs \
	src/TypeChecker/Subsumption.hs \
	src/TypeChecker/Types.hs \
	src/TypeChecker/Unify.hs \
	src/Utils.hs \
	src/Variables.hs

test_objects    := $(test_sources:.hs=.o)
test_interfaces := 

test_packages := $(addprefix -package ,\
	test-framework test-framework-hunit test-framework-quickcheck2 \
	HUnit QuickCheck)

build/bin/dang-tests: GHCFLAGS   += -main-is Tests -hide-all-packages \
                                    $(dang_packages) $(test_packages) \
                                    -fno-warn-missing-signatures \
                                    -fno-warn-orphans
build/bin/dang-tests: LDFLAGS    := -Wall -hide-all-packages $(dang_packages) \
                                    $(test_packages)
build/bin/dang-tests: OBJECTS    := $(test_objects)
build/bin/dang-tests: $(test_objects) | build/bin
	$(call cmd,link_hs)

-include src/.depend.dang-tests

src/.depend.dang-tests: SOURCES := $(test_sources)
src/.depend.dang-tests: $(test_sources)
	$(call cmd,hs_depend)

test:: build/bin/dang-tests
	$Qbuild/bin/dang-tests

clean::
	$Q$(RM) $(test_objects)

mrproper::
	$Q$(RM) src/.depend.dang-tests
