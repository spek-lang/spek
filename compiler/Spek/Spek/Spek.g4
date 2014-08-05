grammar Spek;

/*
Spek Programming Language

Written by William Holroyd
Inspired from...
-- CSharp 3.0 Language Specification
-- Axum 0.8 Language Specification

*/

/* ==================================================
 * === Syntax Rules
 * ================================================== */

// Rules from both CSharp 3.0 and Axum 0.8
// Rules specific to Axum are prefixed with "AX_"

accessModifiedNonArrayType
	: cSharp12Primitives TICK accessModifier
	| scopeType TICK accessModifier
	;

accessModifiedType
	: referenceType TICK accessModifier
	| simpleType TICK accessModifier
	;

accessModifier
	: AX_CONST
	| READ
	| AX_WRITE
	;

accessorBody
	: topLevelBlock
	| SEMICOLON
	;

accessorDeclarations
	: getAccessorDeclaration
	| getAccessorDeclaration? setAccessorDeclaration
	| setAccessorDeclaration setAccessorDeclaration?
	; 

addAccessorDeclaration
	: attributes ADD topLevelBlock
	;

agentAccessModifier
	: AX_READER
	| AX_WRITER
	;

agentBase
	: COLON type COMMA AX_CHANNEL type
	| COLON AX_CHANNEL type
	| COLON AX_CHANNEL type COMMA type
	| COLON type
	;

agentBody
	: OPENBRACE agentMemberDeclaration* CLOSEBRACE
	;

agentConstructorDeclarationDef
	: attributes? memberModifier* agentConstructorDeclarator constructorBody
	;

agentConstructorDeclarator
	: identifier OPENPARENS formalParameterList? CLOSEPARENS constructorInitializer?
	;

agentDeclaration
	: attributes? memberModifier* SEMICOLON agentAccessModifier? 
		AX_PARTIAL? AX_AGENT identifier typeParameterList? 
		agentBase? typeParameterConstraintsClauses? agentBody
	| attributes? agentAccessModifier memberModifier? 
		AX_PARTIAL? AX_AGENT identifier typeParameterList? 
		agentBase? agentBody SEMICOLON
	;

agentFieldDeclaration
	: attributes? memberModifier* concurrencyModifier? type variableDeclarators SEMICOLON
	;

agentMemberDeclaration
	: agentFieldDeclaration
	| agentMethodDeclaration
	| agentConstructorDeclarationDef
	| functionDeclaration
	| delegateDeclarationEx
	;

agentMethodDeclaration
	: agentMethodHeader methodBody
	;

agentMethodHeader
	: attributes? memberModifier* returnType memberName typeParameterList? 
		OPENPARENS formalParameterList? CLOSEPARENS typeParameterConstraintsClauses
	;

anonymousDelegateExpression
	: AX_DELEGATE anonymousMethodSignature? topLevelBlock
	;

anonymousMethodSignature
	: OPENPARENS formalParameterList? CLOSEPARENS
	;

anonymousObjectCreationExpression
	: AX_NEW OPENBRACE memberDeclaratorList? CLOSEBRACE
	| AX_NEW OPENBRACE memberDeclaratorList COMMA CLOSEBRACE
	;

argument
	: expression
	| REF variableReference
	| OUT variableReference
	;

argumentList
	: argument
	| argumentList COMMA argument
	;

arithmeticExpression
	: unaryExpression
	| arithmeticExpression '*' arithmeticExpression
	| arithmeticExpression '/' arithmeticExpression
	| arithmeticExpression '%' arithmeticExpression
	| arithmeticExpression '+' arithmeticExpression
	| arithmeticExpression '-' arithmeticExpression
	;

arrayCreationExpression
	: AX_NEW nonArrayType OPENBRACKET expressionList CLOSEBRACKET rankSpecifier* arrayInitializer?
	| OPENBRACE variableInitializerList CLOSEBRACE
	| AX_NEW arrayType arrayInitializer 
	| AX_NEW rankSpecifier arrayInitializer
	;

arrayInitializer
	: OPENBRACE variableInitializerList? CLOSEBRACE
	| OPENBRACE variableInitializerList? COMMA CLOSEBRACE
	;

arrayType
	: nonArrayType rankSpecifier*
	| accessModifiedNonArrayType rankSpecifier*
	;

assignment
	: unaryExpression assignmentOperatorDef expression
	;

assignmentOperatorDef
	: OP_EQUALASSIGNMENT
	| OP_PLUSEQUALS
	| CLOSEANGLE CLOSEANGLE OP_EQUALASSIGNMENT
	| OP_MINUSEQUALS
	| OP_TIMESEQUALS
	| OP_DIVIDEEQUALS
	| OP_MODULOUSEQUALS
	| OP_ANDEQUALS
	| OP_OREQUALS
	| OP_CARETEQUALS
	| OP_LEFTSHIFTEQUALS
	;

attribute
	: scopeType	attributeArguments?
	;

attributeArgumentExpression
	: expression
	;

attributeArguments
	: OPENPARENS positionalArgumentList CLOSEPARENS
	| OPENPARENS positionalArgumentList COMMA namedArgumentList CLOSEPARENS
	| OPENPARENS namedArgumentList CLOSEPARENS
	;

attributeList
	: attribute
	;

attributes
	: attributeSectionDef*
	;

attributeSectionDef
	: OPENBRACE attributeTarget? attributeList COMMA CLOSEBRACE
	;

attributeTarget
	: FIELD
	| EVENT
	| METHOD
	| PARAM
	| PROPERTY
	| RETURN
	| type
	;

badMethodDeclaration
	: methodHeader methodBody
	;

baseAccess
	: BASE
	;

binaryOperatorDeclarator
	: type OPERATOR overloadableBinaryOperator OPENPARENS type identifier COMMA type identifier CLOSEPARENS
	;

block
	: AX_UNSAFE? OPENBRACE statement* CLOSEBRACE
	;

booleanExpression
	: expression
	;

booleanTypeLiterals
	:
	;

breakStatement
	: BREAK SEMICOLON
	;

castExpression
	: OPENPARENS type CLOSEPARENS unaryExpression
	;

catchClauses
	: specificCatchClause*
	| specificCatchClause* generalCatchClause
	;

channelBase
	: COLON type
	;

channelBody
	: OPENBRACE channelMemberDeclaration CLOSEBRACE
	;

channelDeclaration
	: attributes? memberModifier* AX_PARTIAL? AX_CHANNEL 
		identifier typeParameterList? channelBase? 
		typeParameterConstraintsClauses? channelBody SEMICOLON
	;

channelMemberDeclaration
	: portDeclaration
	| patternStateDeclaration
	| functionDeclaration
	| badMethodDeclaration
	;

characterLiteral
	:
	;

classBase
	: COLON classTypeList
	;

classConstraint
	: classType
	| CLASS
	| STRUCT
	;

classMemberDeclaration
	: constantDeclaration
	| fieldDeclaration
	| methodDeclaration
	| propertyDeclaration
	| eventDeclaration
	| indexerDeclaration
	| operatorDeclaration
	| destructorDeclaration
	| constructorDeclaration
	;

classType
	: predefinedClassType
	| scopeType
	;

classTypeList
	: classType
	| classTypeList COMMA classType
	;

coerceExpression
	: AX_COERCE OPENANGLE type CLOSEANGLE unaryExpression
	;

collectionInitializer
	: OPENBRACE elementInitializerList CLOSEBRACE
	| OPENBRACE elementInitializerList COMMA CLOSEBRACE
	;

commas
	: COMMA
	| commas COMMA
	;

compilationUnit
	: usingDirective* namespaceMemberDeclaration*
	;

concurrencyModifier
	: AX_CONST
	| AX_SYNC
	| AX_ASYNC
	| AX_FUTURE
	;

conditionalExpression
	: relationalExpression
	| relationalExpression OP_QUESTION expression COLON expression
	| relationalExpression OP_COALESCE conditionalExpression
	;

constantDeclaration
	: attributes? memberModifier* AX_CONST type constantDeclarators SEMICOLON
	;

constantDeclarator
	: identifier OP_EQUALSASSIGNMENT constantExpression
	;

constantDeclarators
	: constantDeclarator (COMMA constantDeclarator)*
	;

constantExpression
	: expression
	;

constructorBody
	: topLevelBlock
	| SEMICOLON
	;

constructorDeclaration
	: attributes? memberModifier* constructorDeclarator constructorBody
	;

constructorDeclarator
	: identifier OPENPARENS formalParameterList? CLOSEPARENS constructorInitializer
	;

constructorInitializer
	: COLON BASE OPENPARENS argumentList CLOSEPARENS
	| COLON AX_THIS OPENPARENS argumentList CLOSEPARENS
	;

continueStatement
	: CONTINUE SEMICOLON
	;

conversionOperatorDeclarator
	: IMPLICIT OPERATOR type OPENPARENS type identifier CLOSEPARENS
	| EXPLICIT OPERATOR type OPENPARENS type identifier CLOSEPARENS
	;

cSharp12ParameterTypes
	: cSharp12Primitives
	| arrayType
	;

cSharp12Primitives
	: simpleType
	| predefinedClassType
	; 

cSharpParameterTypes
	: cSharpPrimitives
	| arrayType
	;

cSharpPrimitives
	: cSharp12Primitives
	| simpleType OP_QUESTION
	| predefinedClassType OP_QUESTION
	;

declarationStatement
	: localVariableDeclaration SEMICOLON
	;

defaultValueExpression
	: DEFAULT OPENPARENS type CLOSEPARENS
	;

delegateDeclaration
	: attributes? memberModifier* SEMICOLON 
		AX_DELEGATE returnType identifier typeParameterList? 
		OPENPARENS formalParameterList? CLOSEPARENS typeParameterConstraintsClauses?
	;

delegateDeclarationEx
	: attributes? memberModifier* typeParameterConstraintsClauses? SEMICOLON 
		AX_DELEGATE AX_FUNCTION? returnType identifier typeParameterList? 
		OPENPARENS formalParameterList CLOSEPARENS
	;

destructorDeclaration
	: attributes? EXTERN OP_TILDE identifier OPENPARENS CLOSEPARENS topLevelBlock
	;

dimSeparator
	: COMMA
	;

domainBase
	: COLON type
	;

domainBody
	: OPENBRACE domainMemberDeclaration* CLOSEBRACE
	;

domainConstructorDeclaration
	: attributes? domainConstructorModifier? constructorDeclarator constructorBody
	;

domainConstructorModifier
	: AX_INTERNAL
	| AX_PRIVATE
	| AX_PROTECTED
	| AX_PUBLIC
	;

domainDeclaration
	: attributes? memberModifier* AX_PARTIAL? AX_DOMAIN identifier 
		typeParameterList? domainBase? typeParameterConstraintsClause? domainBody SEMICOLON?
	;

domainFieldDeclaration
	: attributes? domainMemberModifier* concurrencyModifier? type variableDeclarators SEMICOLON
	;

domainMemberDeclaration
	: agentDeclaration
	| channelDeclaration
	| domainDeclaration
	| domainMethodDeclaration
	| domainFieldDeclaration
	| domainConstructorDeclaration
	| functionDeclaration
	| schemaDeclaration
	| delegateDeclarationEx
	| networkDeclaration
	| enumDeclaration
	;

domainMemberModifier
	: ABSTRACT
	| AX_NEW
	| AX_VOLATILE
	| ASYNCHRONOUS
	| OVERRIDE
	| AX_PRIVATE
	| AX_PROTECTED
	| READONLY
	| AX_SEALED
	| STATIC
	| AX_UNSAFE
	| AX_VIRTUAL
	;

domainMethodDeclaration
	: domainMethodHeader methodBody
	;

domainMethodHeader
	: attributes? domainMemberModifier* returnType memberName typeParameterList? 
		OPENPARENS formalParameterList? CLOSEPARENS typeParameterConstraintsClauses?
	;

doStatement
	: DO embeddedStatement WHILE OPENPARENS booleanExpression CLOSEPARENS
	;

elementAccess
	: primaryNoArrayCreationExpression OPENBRACKET expressionList CLOSEBRACKET 
	;

elementInitializer
	: nonAssignmentExpression
	| OPENBRACE expression CLOSEBRACE
	;

elementInitializerList
	: elementInitializer
	;

embeddedStatement
	: block
	| embeddedStatementNoBlock
	;

embeddedStatementNoBlock
	: epsStatement
	| expressionStatement
	| receiveStatement
	| interleaveStatement
	| vectorInterleaveStatement
	| networkExpression
	| selectionStatement
	| iterationStatement
	| jumpStatement
	| tryStatement
	| overflowControlStatement
	| usingStatement
	| fixedStatement
	| requireStatement
	;

enumBase
	: COLON integralType
	;

enumBody
	: OPENBRACE enumMemberDeclarations? COMMA? CLOSEBRACE
	;

enumDeclaration
	: attributes? memberModifier* AX_ENUM identifier enumBase? enumBody SEMICOLON?
	;

enumMemberDeclaration
	: attributes? identifier
	| attributes? identifier OP_EQUALSASSIGNMENT constantExpression
	;

enumMemberDeclarations
	: enumMemberDeclaration (COMMA enumMemberDeclaration)*
	;

epsStatement
	: SEMICOLON
	;

eventAccessorDeclarations
	: addAccessorDeclaration removeAccessorDeclaration
	| removeAccessorDeclaration addAccessorDeclaration
	;

eventDeclaration
	: attributes? memberModifier* EVENT type variableDeclarators SEMICOLON
	| attributes? memberModifier* EVENT type memberName 
		OPENBRACE eventAccessorDeclarations CLOSEBRACE
	;

explicitlyTypedLambdaParameter
	: parameterModifiers? type identifier
	;

explicitlyTypedLambdaParameterList
	: explicitlyTypedLambdaParameter*
	;

expression
	: nonAssignmentExpression
	| assignment
	;

expressionList
	: expression
	| expressionList COMMA expression
	;

expressionStatement
	: primaryStatementExpression SEMICOLON
	| unaryStatementExpression SEMICOLON
	| assignment SEMICOLON
	;

fieldDeclaration
	: attributes?
	| memberModifier* type variableDeclarators SEMICOLON
	;

finallyClause
	: FINALLY block
	;

fixedParameter
	: attributes? parameterModifier? concurrencyModifier? type identifier
	;

fixedParameters
	: fixedParameter (COMMA fixedParameter)*
	;

fixedStatement
	: FIXED	OPENPARENS uPointerType identifier OP_EQUALSASSIGNMENT expression CLOSEPARENS embeddedStatement
	;

floatingPointType
	: FLOAT
	| DOUBLE
	;

forCondition
	: booleanExpression
	;

foreachStatement
	: FOREACH OPENPARENS typeInferenceType identifier IN expression CLOSEPARENS embeddedStatement
	;

forInitializer
	: localVariableDeclaration
	| statementExpressionList
	;

forIterator
	: statementExpressionList
	;

formalParameterList
	: fixedParameters
	| fixedParameters COMMA parameterArray
	| parameterArray
	;

forStatement
	: FOR OPENPARENS forInitializer? SEMICOLON forCondition? SEMICOLON forIterator? CLOSEPARENS embeddedStatement
	;

fromClause
	: FROM type? identifier IN expression
	;

functionDeclaration
	: functionHeader methodBody
	;

functionHeader
	: attributes? memberModifier* AX_FUNCTION returnType memberName typeParameterList 
		OPENPARENS formalParameterList CLOSEPARENS typeParameterConstraintsClauses
	;

generalCatchClause
	: CATCH block
	;

genericDimensionSpecifier
	: OPENANGLE commas? CLOSEANGLE
	;

genericMethodExpression
	: simpleName OPENANGLE typeArguments CLOSEANGLE
	| memberAccess OPENANGLE typeArguments CLOSEANGLE
	;

genericType
	: scopeType DOT identifier OPENANGLE typeArguments CLOSEANGLE
	;

getAccessorDeclaration
	: attributes? memberModifier* GET accessorBody
	;

globalAttributeSection
	: OPENBRACKET globalAttributeTargetSpecifier attributeList COMMA? CLOSEBRACKET
	;

globalAttributeTarget
	: AX_ASSEMBLY
	| AX_MODULE
	;

globalAttributeTargetSpecifier
	: globalAttributeTarget COLON
	;

gotoStatement
	: GOTO identifier SEMICOLON
	| GOTO CASE constantExpression SEMICOLON
	| GOTO DEFAULT SEMICOLON
	;

groupClause
	: AX_GROUP expression AX_BY expression
	;

identifier
	: '@'? LETTER NUMBER*
	;

ifStatement
	: IF OPENPARENS booleanExpression CLOSEPARENS nestedStatement
	| IF OPENPARENS booleanExpression CLOSEPARENS nestedStatement ELSE embeddedStatement
	;

implicitlyTypedLambdaParameter
	: identifier
	;

implicitlyTypedLambdaParameterList
	: implicitlyTypedLambdaParameter*
	;

indexerDeclaration
	: attributes? memberModifier* indexerDeclarator OPENBRACE accessorDeclarations CLOSEBRACE
	;

indexerDeclarator
	: type AX_THIS OPENBRACKET formalParameterList CLOSEBRACKET
	| type scopeType DOT AX_THIS OPENBRACKET formalParameterList CLOSEBRACKET
	;

integralType
	: SBYTE
	| BYTE
	| SHORT
	| USHORT
	| INT
	| UINT
	| LONG
	| ULONG
	| CHAR
	;

integerLiteral
	: decimalIntegerLiteral
	| hexadecimalIntegerLiteral
	;

integerTypeSuffix
	: unsignedSuffix
	| longSuffix
	| unsignedSuffix longSuffix
	| longSuffix unsignedSuffix
	;

unsignedSuffix
	: 'u'
	| 'U'
	;

longSuffix
	: 'l'
	| 'L'
	;

hexPrefix
	: '0x'
	| '0X'
	;

hexDigit
	: LETTER
	| decimalDigit
	;

decimalIntegerLiteral
	: decimalLiteral integerTypeSuffix?
	;

decimalDigit
	: NUMBER*
	;

decimalLiteral
	: NUMBER*
	;

hexadecimalIntegerLiteral
	: hexPrefix hexDigit* integerTypeSuffix?
	;

interfaceConstraint
	: interfaceType
	;

interfaceConstraints
	: interfaceConstraint (COMMA interfaceConstraint)*
	;

interfaceType
	: scopeType
	;

interfaceTypeList
	: scopeType (COMMA scopeType)*
	;

interleaveStatement
	: INTERLEAVE OPENBRACE statement* CLOSEBRACE
	;

intoClause
	: INTO identifier queryBody
	;

invocationArguments
	: OPENPARENS argumentList? CLOSEPARENS
	;

invocationExpression
	: primaryExpression invocationArguments
	;

iterationStatement
	: whileStatement
	| doStatement
	| forStatement
	| foreachStatement
	;

joinClause
	: AX_JOIN type? identifier IN expression AX_ON expression AX_EQUALS expression
	;

joinIntoClause
	: AX_JOIN type? identifier identifier IN expression AX_ON expression AX_EQUALS expression INTO
	;

jumpStatement
	: breakStatement
	| continueStatement
	| gotoStatement
	| yieldReturnStatement
	| returnStatement
	| throwStatement
	;

labeledStatement
	: identifier COLON statement
	;

lambdaExpression
	: OPENPARENS lambdaParameterList? CLOSEPARENS OP_LAMBDAARROW lambdaExpressionBody
	| implicitlyTypedLambdaParameter OP_LAMBDAARROW lambdaExpressionBody
	;

lambdaExpressionBody
	: expression
	| block
	;

lambdaParameterList
	: explicitlyTypedLambdaParameterList
	| implicitlyTypedLambdaParameterList
	;

layoutDeclaration
	: AX_LAYOUT OPENBRACE statement* CLOSEBRACE
	;

letClause
	: AX_LET identifier OP_EQUALSASSIGNMENT expression
	;

literal
	: booleanTypeLiterals
	| integerLiteral
	| realLiteral
	| characterLiteral
	| stringLiteral
	| nullLiteral
	;

localVariableDeclaration
	: concurrencyModifier? typeInferenceType localVariableDeclarators
	;

localVariableDeclarator
	: identifier 
	| identifier OP_EQUALSASSIGNMENT localVariableInitializer
	;

localVariableDeclarators
	: localVariableDeclarator (COMMA localVariableDeclarator)
	;

localVariableInitializer
	: expression
	;

memberAccess
	: memberAccessContainerExpression DOT identifier
	| memberAccessContainerExpression AX_PROJECTION identifier
	| predefinedType DOT identifier
	| genericType DOT identifier
	;

memberAccessContainerExpression
	: specialMemberAccessContainerExpression
	| primaryNoGenericMethodExpression
	;

memberDeclarator
	: simpleName
	| memberAccess
	| identifier OP_EQUALSASSIGNMENT expression
	;

memberDeclaratorList
	: memberDeclarator
	;

memberInitializer
	: identifier OP_EQUALSASSIGNMENT expression
	| identifier OP_EQUALSASSIGNMENT objectOrCollectionInitializer
	;

memberInitializerList
	: memberInitializer
	;

memberModifier
	: ABSTRACT
	| EXTERN
	| STATIC
	| AX_UNSAFE
	| AX_VIRTUAL
	| AX_VOLATILE
	| ASYNCHRONOUS
	| AX_INTERNAL
	| AX_NEW
	| OVERRIDE
	| AX_PRIVATE
	| AX_PROTECTED
	| AX_PUBLIC
	| READONLY
	| SEALED
	;

memberName
	: identifier
	| scopeType DOT identifier
	;

methodBody
	: topLevelBlock
	| SEMICOLON
	;

methodDeclaration
	: methodHeader methodBody
	;

methodHeader
	: attributes? memberModifier* returnType memberName typeParameterList? 
		OPENPARENS formalParameterList CLOSEPARENS typeParameterConstraintsClauses
	;

namedArgument
	: identifier OP_EQUALSASSIGNMENT attributeArgumentExpression
	;

namedArgumentList
	: namedArgument (COMMA namedArgument)*
	;

namespaceBody
	: OPENBRACE usingDirective* namespaceMemberDeclaration* CLOSEBRACE
	;

namespaceDeclaration
	: namespace namespaceName namespaceBody
	;

namespaceMemberDeclaration
	: namespaceDeclaration
	| agentDeclaration
	| channelDeclaration
	| domainDeclaration
	| schemaDeclaration
	| networkDeclaration
	| enumDeclaration
	;

namespaceName
	: qualifiedName
	| namespaceStringLiteral
	;

namespacesStringLiteral
	: regularStringLiteral
	| verbatimStringLiteral
	;

namespacesTypeReference
	: scopeType 
	| namespaceStringLiteral
	;

nestedStatement
	: embeddedStatement
	;

networkBody
	: OPENBRACE networkMemberDeclaration CLOSEBRACE
	;

networkConstructorDeclaration
	: attributes? memberModifier* constructorDeclarator constructorBody
	;

networkDeclaration
	: attributes? membermodifier* AX_PARTIAL? AX_NETWORK identifier typeParameterList?
		classBase? typeParameterConstrainstClauses networkBody
	;

networkExpression
	: relationalExpression AX_SEND nonAssignmentExpression
	| relationalExpression AX_FORWARD nonAssignmentExpression
	| relationalExpression AX_FORWARDONCE nonAssignmentExpression
	| relationalExpression AX_BROADCAST nonAssignmentExpression
	| relationalExpression AX_ALTERNATE nonAssignmentExpression
	| relationalExpression AX_COMBINE nonAssignmentExpression
	| relationalExpression AX_MULTIPLEX nonAssignmentExpression
	;

networkFieldDeclaration
	: attributes? memberModifier* concurrenyModifier? type variableDeclarators SEMICOLON
	; 

networkMemberDeclaration
	: networkFieldDeclaration
	| methodDeclaration
	| networkConstructorDeclaration
	| functionDeclaration
	| delegateDeclarationEx
	| layoutDeclaration
	| networkPortDeclaration
	| patternStateDeclaration
	;

networkPortDeclaration
	: attributes? memberModifier* portKind type variableDeclarator SEMICOLON
	| attributes? memberModifier* portKind type OP_EQUALSASSIGNMENT expression SEMICOLON
	| attributes? memberModifier* portKind type identifier COLON type OP_EQUALSASSIGNMENT expression SEMICOLON
	;

newConstraint
	: AX_NEW OPENPARENS CLOSEPARENS
	;

nonArrayType
	: cSharp12Primitives
	| scopeType
	| uPointerType
	;

nonAssignmentExpression
	: lambdaExpression
	| queryExpression
	| conditionalExpression
	| networkExpression
	;

nullableType
	: type OP_QUESTION
	;

nullLiteral
	: NULL
	;

numericType
	: integralType
	| floatingPointType
	| DECIMAL
	;

objectDelegateCreationExpression
	: AX_NEW type OPENPARENS argumentList? CLOSEPARENS
	| AX_NEW type OPENPARENS argumentList? CLOSEPARENS objectOrCollectionInitializer
	| AX_NEW nonArrayType objectOrCollectionInitializer
	;

objectInitializer
	: OPENBRACE memberInitializerList? CLOSEBRACE
	| OPENBRACE memberInitializerList? COMMA CLOSEBRACE
	;

objectOrCollectionInitializer
	: objectInitializer
	| collectionInitializer
	;

operatorBody
	: topLevelBlock
	;

operatorDeclaration
	: attributes? memberModifier* operatorDeclarator operatorBody
	;

operatorDeclarator
	: binaryOperatorDeclarator
	| conversionOperatorDeclarator
	| unaryOperatorDeclarator
	;

orderByClause
	: AX_ORDERBY ordering*
	;

ordering
	: expression
	| expression AX_ASCENDING
	| expression AX_DESCENDING
	;

overflowControlExpression
	: CHECKED OPENPARENS expression CLOSEPARENS
	| UNCHECKED OPENPARENS expression CLOSEPARENS
	;

overflowControlStatement
	: CHECKED block
	| UNCHECKED block
	;

overloadableBinaryOperator
	: OP_PLUS
	| OP_MINUS
	| OPENANGLE
	| OP_LESSTHANOREQUALS
	| CLOSEANGLE
	| OP_GREATERTHANOREQUALS
	| OP_EQUALCOMPARISON
	| OP_NOTEQUALCOMPARISON
	| ASTERISK
	| OP_FORWARDSLASH
	| OP_PERCENT
	| OP_AMPERSAND
	| OP_STICK
	| OP_CARET
	| OP_DOUBLEOPENANGLE
	| CLOSEANGLE CLOSEANGLE
	;

overloadableUnaryOperator
	: OP_PLUS
	| OP_MINUS
	| OP_EXCLAMATION
	| OP_TILDE
	| OP_INCREMENT
	| OP_DECREMENT
	;

parameterArray
	: attributes? PARAMS arrayType identifier
	;

parameterModifier
	: REF
	| OUT
	| AX_THIS
	;

parentAccess
	: AX_PARENT
	;

parenthesizedExpression
	: OPENPARENS expression CLOSEPARENS
	;

parenthesizedTriggerExpression
	: OPENPARENS triggerExpression CLOSEPARENS
	;

partial
	: AX_PARTIAL
	;

patternStateDeclaration
	: identifier COLON OPENBRACE transitionStatement* CLOSEBRACE SEMICOLON
	| identifier COLON ABSTRACT SEMICOLON
	;

portDeclaration
	: attributes? memberModifier* portKind? type variableDeclarators SEMICOLON
	| attributes? memberModifier* portKind? type variableDeclarators COLON type SEMICOLON
	;

portKind
	: AX_INPUT 
	| AX_OUTPUT
	;

portName
	: identifier
	;

positionalArgument
	: attributeArgumentExpression
	;

positionalArgumentList
	: PositionalArgument (COMMA PositionalArgument)*
	;

postDecrementExpression
	: primaryExpression OP_DECREMENT
	;

postIncrementExpression
	: primaryExpression OP_INCREMENT
	;

preDecrementExpression
	: OP_DECREMENT unaryExpression
	;

preIncrementExpression
	: OP_INCREMENT unaryExpression
	;

predefinedType
	: simpleType
	| predefinedClassType
	;

predefinedClassType
	: OBJECT
	| STRING
	;

primaryExpression
	: primaryNoGenericMethodExpression
	| genericMethodExpression
	| specialMemberAccessContainerExpression
	;

primaryNoArrayCreationExpression
	: literal
	| simpleName
	| overflowControlExpression
	| parenthesizedExpression
	| memberAccess
	| primaryStatementExpression
	| sizeofExpression
	| elementAccess
	| anonymousDelegateExpression
	| defaultValueExpression
	| typeofExpression
	;

primaryNoGenericMethodExpression
	: primaryNoArrayCreationExpression
	| arrayCreationExpression
	| anonymousObjectCreationExpression
	;

primaryStatementExpression
	: invocationExpression
	| receiveExpression
	| tryReceiveExpression
	| objectDelegateCreationExpression
	| postIncrementExpression
	| postDecrementExpression
	;

primaryTriggerExpression
	: portName
	| portName AX_DOLLAR parenthesizedExpression
	| parenthesizedTriggerExpression
	;

propertyDeclaration
	: attributes? memberModifier* type memberName OPENBRACE accessorDeclarations CLOSEBRACE
	;

qualifiedName
	: identifier
	| qualifiedName DOT identifier
	;

queryBody
	: queryBodyClauses? selectClause intoClause?
	| queryBodyClauses? groupClause intoClause?
	;

queryBodyClause
	: fromClause
	| letClause
	| whereClause
	| joinClause
	| joinIntoClause
	| orderByClause
	;

queryBodyClauses
	: queryBodyClause (queryBodyClause)*
	;

queryExpression
	: fromClause queryBody
	;

rankSpecifier
	: OPENBRACKET dimSeparator CLOSEBRACKET
	;

realLiteral
	: 
	;

receiveBlock
	: OPENBRACE receiveSection* CLOSEBRACE
	;

receiveExpression
	: AX_RECEIVE OPENPARENS expression CLOSPARENS
	;

receiveIntoClause
	: INTO expression
	;

receiveSection
	: REPEAT? FROM expression receiveIntoClause COLON statement*
	;

receiveStatement
	: AX_RECEIVE receiveBlock
	;

referenceType
	: predinfedClassType
	| scopeType
	| arrayType
	;

relationalExpression
	: shiftExpression
	| relationalExpression OPENANGLE relationalExpression
	| relationalExpression OP_CARET relationalExpression
	| relationalExpression OP_STICK relationalExpression
	| relationalExpression OP_DOUBLEAMPERSAND relationalExpression
	| relationalExpression OP_DOUBLESTICK relationalExpression
	| relationalExpression CLOSEANGLE relationalExpression
	| relationalExpression OP_LESSTHANOREQUALS relationalExpression
	| relationalExpression OP_GREATERTHANOREQUALS relationalExpression
	| relationalExpression AX_IS relationalExpression
	| relationalExpression AX_AS relationalExpression
	| relationalExpression OP_EQUALSCOMPARISON relationalExpression
	| relationalExpression OP_NOTEQUALSCOMPARISON relationalExpression
	| relationalExpression OP_AMPERSAND relationalExpression
	;

removeAccessorDeclaration
	: attributes? REMOVE topLevelBlock
	;

requireStatement
	: AX_REQUIRE expression SEMICOLON
	;

returnType
	: concurrencyModifier? type
	;

ruleDeclaration
	:
	;

resourceAcquisition
	: localVariableDeclaration
	| expression
	;

returnStatement
	: RETURN expression? SEMICOLON
	;

rulesBody
	: OPENBRACE statement* CLOSEBRACE
	;

schemaBase
	: COLON type
	;

schemaBody
	: OPENBRACE schemaMemberDeclaration CLOSEBRACE
	;

schemaDeclaration
	: attributes? memberModififer* AX_PARTIAL? AX_SCHEMA identifier typeParameterList? schemaBase? typeParameterConstraintsClauses? schemaBody SEMICOLON?
	;

schemaFieldAttribute
	: AX_REQUIRED
	| AX_OPTIONAL
	;

schemaFieldAttributeEx
	: schemaFieldAttribute?
	| memberModifier?
	| memberModifier? schemaFieldAttribute
	| schemaFieldAttribute memberModifier? 
	;

schemaFieldDeclaration
	: schemaFieldAttributeEx type identifier SEMICOLON
	;

schemaMemberDeclaration
	: schemaFieldDeclaration
	| functionDeclaration
	| ruleDeclaration
	;

scopeType
	: identifier
	| regularStringLiteral DOT identifier
	| scopeType DOT indentifier
	| genericType
	;

selectClause
	: AX_SELECT expression
	;

selectionStatement
	: ifStatement 
	| switchStatement
	;

setAccessorDeclaration
	: attributes? memberModifier* SET accessorBody
	;

shiftExpression
	: arithmeticExpression
	| shiftExpression OP_DOUBLEOPENANGLE shiftExpression
	| shiftExpression CLOSEANGLE CLOSEANGLE arithmeticExpression
	;

simpleName
	: identifier
	;

simpleType
	: numericType
	| booleanType
	;

sizeofExpression
	: SIZEOF OPENPARENS type CLOSEPARENS
	;

specialMemberAccessContainerExpression
	: thisAccess
	| baseAccess
	| parentAccess
	;

specificCatchClause
	: CATCH OPENPARENS classType identifier? CLOSEPARENS block
	;

statement
	: labeledStatement
	| declarationStatement
	| embeddedStatement
	| SEMICOLON
	;

statementExpression
	: primaryStatementExpression
	| unaryStatementExpression
	| assignment
	;

statementExpressionList
	: statementExpression (COMMA statementExpression)*
	;

stringLiteral
	: regularStringLiteral
	| verbatimStringLiteral
	;

structBody
	: OPENBRACE structMemberDeclaration CLOSEBRACE
	;

structDeclaration
	: attributes? memberModifier* PARTIAL? STRUCT identifier typeParameterlist? structInterfaces? typeParameterConstrainstClauses
	;

structInterfaces
	:
	;

structMemberDeclaration
	:
	;

switchBlock
	:
	;

switchLabel
	:
	;

switchSection
	:
	;

switchStatement
	:
	;

thisAccess
	:
	;

throwStatement
	:
	;

topLevelBlock
	: OPENBRACE statement* CLOSEBRACE
	;

transitionStatement
	:
	;

triggerExpression
	:
	;

tryReceiveExpression
	:
	;

tryStatement
	:
	;

type
	:
	;

typeArguments
	:
	;

typeInferenceType
	:
	;

typeofExpression
	:
	;

typeParameter
	:
	;

typeParameterConstraints
	:
	;

typeParameterConstraintsClause
	:
	;

typeParameterConstraintsClauses
	:
	;

typeParameterList
	:
	;

typeParameters
	:
	;

unaryExpression
	:
	;

unaryOperatorDeclarator
	:
	;

unaryStatementExpression
	:
	;

unboundTypeName
	:
	;

uPointerType
	:
	;

usingAliasDirective
	:
	;

usingDirective
	:
	;

usingNamespaceDirective
	:
	;

usingStatement
	:
	;

variableDeclarator
	:
	;

variableDeclarators
	:
	;

variableInitializer
	:
	;

variableInitializerList
	:
	;

variableReference
	:
	;

vectorInterleaveStatement
	:
	;

whereClause
	:
	;

whileStatement
	:
	;

yieldBreakStatement
	:
	;

yieldReturnStatement
	:
	;

/* ==================================================
 * === Keywords
 * ================================================== */

// Keywords from both CSharp 3.0 and Axum 0.8
// Keywords specific to Axum are prefixed with "AX_"

ABSTRACT 
	: 'abstract'
	;

ADD
	: 'add'
	;

ASCENDING
	: 'ascending'
	;

AS
	: 'as'
	;

ASSEMBLY
	: 'assembly'
	;

ASYNCHRONOUS
	: 'asynchronous'
	;

BASE
	: 'base'
	;

BOOL
	: 'bool'
	;

BREAK
	: 'break'
	;

BY
	: 'by'
	;

BYTE
	: 'byte'
	;

CASE
	: 'case'
	;

CATCH
	: 'catch'
	;

CHAR
	: 'char'
	;

CHECKED
	: 'checked'
	;

CLASS
	: 'class'
	;

CONST
	: 'const'
	;

CONTINUE
	: 'continue'
	;

DECIMAL
	: 'decimal'
	;

DEFAULT
	: 'default'
	;

DELEGATE
	: 'delegate'
	;

DESCENDING
	: 'descending'
	;

DO
	: 'do'
	;

DOUBLE
	: 'double'
	;

ELSE
	: 'else'
	;

ENUM
	: 'enum'
	;

EQUALS
	: 'equals'
	;

EVENT
	: 'event'
	;

EXPLICIT
	: 'explicit'
	;

EXTERN
	: 'extern'
	;

FALSE
	: 'false'
	;

FIELD
	: 'field'
	;

FINALLY
	: 'finally'
	;

FIXED
	: 'fixed'
	;

FLOAT
	: 'float'
	;

FOREACH
	: 'foreach'
	;

FOR
	: 'for'
	;

FROM
	: 'from'
	;

GET
	: 'get'
	;

GLOBAL
	: 'global'
	;

GOTO
	: 'goto'
	;

GROUP
	: 'group'
	;

IF
	: 'if'
	;

IMPLICIT
	: 'implicit'
	;

IN
	: 'in'
	;

INTERFACE
	: 'interface'
	;

INTERLEAVE
	: 'interleave'
	;

INTERNAL
	: 'internal'
	;

INT
	: 'int'
	;

INTO
	: 'into'
	;

IS
	: 'is'
	;

JOIN
	: 'join'
	;

LET
	: 'let'
	;

LOCK
	: 'lock'
	;

LONG
	: 'long'
	;

METHOD
	: 'method'
	;

MODEL
	: 'model'
	;

MODULE
	: 'module'
	;

NAMESPACE
	: 'namespace'
	;

NEW
	: 'new'
	;

NULL
	: 'null'
	;

OPERATOR
	: 'operator'
	;

OUT
	: 'out'
	;

OVERRIDE
	: 'override'
	;

PARAM
	: 'param'
	;

PARAMS
	: 'params'
	;

PARENT
	: 'parent'
	;

PARTIAL
	: 'partial'
	;

PRIVATE
	: 'private'
	;

PROPERTY
	: 'property'
	;

PROTECTED
	: 'protected'
	;

PUBLIC
	: 'public'
	;

READONLY
	: 'readonly'
	;

READ
	: 'read'
	;

REF
	: 'ref'
	;

REMOVE
	: 'remove'
	;

REPEAT
	: 'repeat'
	;

RETURN
	: 'return'
	;

SBYTE
	: 'sbyte'
	;

SEALED
	: 'sealed'
	;

SELECT
	: 'select'
	;

SET
	: 'set'
	;

SHORT
	: 'short'
	;

SIZEOF
	: 'sizeof'
	;

STATIC
	: 'static'
	;

STRING
	: 'string'
	;

STRUCT
	: 'struct'
	;

SWITCH
	: 'switch'
	;

THIS
	: 'this'
	;

THROW
	: 'throw'
	;

WHILE
	: 'while'
	;

// From Axum 0.8 Specification

AX_AGENT
	: 'agent'
	;

AX_AS
	: 'as'
	;

AX_ASCENDING
	: 'ascending'
	;

AX_ASSEMBLY
	: 'assembly'
	;

AX_ASYNC
	: 'async'
	;

AX_BY
	: 'by'
	;

AX_CHANNEL
	: 'channel'
	;

AX_COERCE
	: 'coerce'
	;

AX_CONST
	: 'const'
	;

AX_DELEGATE
	: 'delegate'
	;

AX_DESCENDING
	: 'descending'
	;

AX_DOMAIN
	: 'domain'
	;

AX_ENUM
	: 'enum'
	;

AX_EQUALS
	: 'equals'
	;

AX_FUNCTION
	: 'function'
	;

AX_FUTURE
	: 'future'
	;

AX_GROUP
	: 'group'
	;

AX_INPUT
	: 'input'
	;

AX_INTERNAL
	: 'internal'
	;

AX_IS
	: 'is'
	;

AX_LAYOUT
	: 'layout'
	;

AX_LET
	: 'let'
	;

AX_MODULE
	: 'module'
	;

AX_NETWORK
	: 'network'
	;

AX_NEW
	: 'new'
	;

AX_ON
	: 'on'
	;

AX_OPTIONAL
	: 'optional'
	;

AX_ORDERBY
	: 'orderby'
	;

AX_OUTPUT
	: 'output'
	;

AX_PARENT
	: 'parent'
	;

AX_PARTIAL
	: 'partial'
	;

AX_PRIVATE
	: 'private'
	;

AX_PROTECTED
	: 'protected'
	;

AX_PUBLIC
	: 'public'
	;

AX_READER
	: 'reader'
	;

AX_RECEIVE
	: 'receive'
	;

AX_REQUIRE
	: 'require'
	;

AX_REQUIRED
	: 'required'
	;

AX_RULES
	: 'rules'
	;

AX_SCHEMA
	: 'schema'
	;

AX_SEALED
	: 'sealed'
	;

AX_SELECT
	: 'select'
	;

AX_STATE
	: 'state'
	;

AX_SYNC
	: 'sync'
	;

AX_THIS
	: 'this'
	;

AX_WRITER
	: 'writer'
	;

AX_TRUE
	: 'true'
	;

AX_TRY
	: 'try'
	;

AX_TYPE
	: 'type'
	;

AX_TYPEOF
	: 'typeof'
	;

AX_UINT
	: 'uint'
	;

AX_ULONG
	: 'ulong'
	;

AX_UNCHECKED
	: 'unchecked'
	;

AX_UNSAFE
	: 'unsafe'
	;

AX_USHORT
	: 'ushort'
	;

AX_USING
	: 'using'
	;

AX_VAR
	: 'var'
	;

AX_VIRTUAL
	: 'virtual'
	;

AX_VOID
	: 'void'
	;

AX_VOLATILE
	: 'volatile'
	;

AX_WHERE
	: 'where'
	;

AX_WRITE
	: 'write'
	;

AX_YIELD
	: 'yield'
	;

/* ==================================================
 * === Operators
 * ================================================== */

// From C Sharp 3.0 Specification

OP_AMPERSAND
	: '&'
	;

OP_ANDEQUALS
	: '&='
	;

OP_ASTERISK
	: '*'
	;

OP_CARET
	: '^'
	;

OP_CARETEQUALS
	: '^='
	;

OP_COALESCE
	: '??'
	;

OP_DECREMENT
	: '--'
	;

OP_DIVIDEEQUALS
	: '/='
	;

OP_DOUBLEOPENANGLE
	: '<<'
	;

OP_EQUALASSIGNMENT
	: '='
	;

OP_EQUALCOMPARISON
	: '=='
	;

OP_EXCLAMATION
	: '!'
	;

OP_FORWARDSLASH
	: '/'
	;

OP_GREATERTHANOREQUALS
	: '>'
	;

OP_INCREMENT
	: '++'
	;

OP_LAMBDAARROW
	: '=>'
	;

OP_LEFTSHIFTEQUALS
	: '<<='
	;

OP_LESSTHANOREQUALS
	: '<'
	;

OP_MINUS
	: '-'
	;

OP_MINUSEQUALS
	: '-='
	;

OP_MODULOUSEQUALS
	: '%='
	;

OP_NOTEQUALCOMPARISON
	: '!='
	;

OP_OREQUALS
	: '|='
	;

OP_PERCENT
	: '%'
	;

OP_PLUS
	: '+'
	;

OP_PLUSEQUALS
	: '+='
	;

OP_QUESTION
	: '?'
	;

OP_RIGHTARROW
	: '->'
	;

OP_STICK
	: '|'
	;

OP_TILDE
	: '~'
	;

OP_TIMESEQUALS
	: '*='
	;

// From Axum 0.8 Specification

AX_OP_ALTERNATE
	: '-<:'
	;

AX_OP_BROADCAST
	: '-<<'
	;

AX_OP_COMBINE
	: '&>-'
	;

AX_OP_DOLLAR
	: '$'
	;

AX_OP_FORWARD
	: '==>'
	;

AX_OP_FORWARDONCE
	: '-->'
	;

AX_OP_MULTIPLEX
	: '>>-'
	;

AX_OP_PROJECTION
	: '::'
	;

AX_OP_SEND
	: '<--'
	;

/* ==================================================
 * === Delimiters
 * ================================================== */

// From C Sharp 3.0 Specification

CLOSEANGLE
	: '>'
	;

CLOSEBRACE
	: '}'
	;

CLOSEBRACKET
	: ']'
	;

CLOSEPARENS
	: ')'
	;

COLON
	: ':'
	;

COMMA
	: ','
	;

DOT
	: '.'
	;

DOUBLEAMPERSAND
	: '&&'
	;

DOUBLECOLON
	: '::'
	;

DOUBLESTICK
	: '||'
	;

OPENANGLE
	: '<'
	;

OPENBRACE
	: '{'
	;

OPENPARENS
	: '('
	;

OPENBRACKET
	: '['
	;

SEMICOLON
	: ';'
	;

TICK
	: '`'
	;

LETTER
	: [a-zA-Z_]
	;


NUMBER
	: [0-9]
	;
