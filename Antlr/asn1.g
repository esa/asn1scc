/*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*/

grammar asn1;
options {
	output=AST;
	language=CSharp2;
}


/*
Parser tokens
*/

tokens {
	MODULE_DEF;
	TYPE_ASSIG;
	VAL_ASSIG;
	VAL_SET_ASSIG;
	TYPE_DEF;
	TYPE_TAG;
	BIT_STRING_TYPE;
	BIT_STRING_ITEM;
	ENUMERATED_LST_ITEM;
	ENUMERATED_TYPE;
	INTEGER_TYPE;
	INTEGER_LST_ITEM;
	CHOICE_TYPE;
	CHOICE_EXT_BODY;
	CHOICE_ITEM;
	CHOICE_EXT_ITEM;
	SEQUENCE_TYPE;
	SET_TYPE;
	SEQUENCE_BODY;
	SEQUENCE_EXT_BODY;
	SEQUENCE_ITEM;
	SEQUENCE_EXT_GROUP;
	SEQUENCE_OF_TYPE;
	SET_OF_TYPE;
	STRING_TYPE;
	TIME_TYPE;
	REFERENCED_TYPE;
	OBJ_LST_ITEM1;
	OBJ_LST_ITEM2;
	DEFINED_VALUE;
	CONSTRAINT;
	EXCEPTION_SPEC;
	UNION_ELEMENT2;
	UNION_SET_ALL_EXCEPT;
	INTERSECTION_ELEMENT;
	VALUE_RANGE_EXPR;
	SUBTYPE_EXPR;
	SIZE_EXPR;
	PERMITTED_ALPHABET_EXPR;
	INNER_TYPE_EXPR;
	NAME_CONSTRAINT_EXPR;
	PATTERN_EXPR;
	MIN_VAL_INCLUDED;
	MAX_VAL_INCLUDED;
	MAX_VAL_PRESENT;
	CHAR_SEQUENCE_VALUE;
	EXPORTS_ALL;
	IMPORTS_FROM_MODULE;
	ASN1_FILE;
	OCTECT_STING;
	SIMPLIFIED_SIZE_CONSTRAINT;
	OBJECT_TYPE;
	NUMBER_LST_ITEM;
	DEFAULT_VALUE;
	VALUE_REFERENCE;
	COMPONENTS_OF;
	NAMED_VALUE;
	NAMED_VALUE_LIST;
	VALUE_LIST;
	CHOICE_VALUE;
	OBJECT_ID_VALUE;
	UNION_SET;
	INTERSECTION_SET;
	NUMERIC_VALUE2;
	SELECTION_TYPE;
	WITH_COMPONENT_CONSTR;
	WITH_COMPONENTS_CONSTR;
	EXCEPTION_SPEC_CONST_INT;
	EXCEPTION_SPEC_VAL_REF;
	EXCEPTION_SPEC_TYPE_VALUE;
	EMPTY_LIST;
	ALL_EXCEPT;
	SINGLE_VALUE_CONSTRAINT;
	INNER_CONSTRAINT;
	PARAM_LIST;
	TYPE_PARAM;
	VALUE_PARAM;
	ACTUAL_PARAM_LIST;
	ACTUAL_TYPE_PARAM;
	ACTUAL_VALUE_PARAM;
	PREFERENCED_TYPE;
}

@parser::namespace { Antlr.Asn1 } // Or just @namespace { ... }

@lexer::namespace { Antlr.Asn1 }


@header {
#pragma warning disable 0219
}

@members {
public class SyntaxErrorException : Exception
{
    public SyntaxErrorException(string msg) : base(msg) { }
}

/*
public override void ReportError(RecognitionException e) {
	base.ReportError(e);
        throw e;
}*/

public override void EmitErrorMessage(string msg)
{
    throw new SyntaxErrorException(msg);
//    throw new Semantix.Utils.SyntaxErrorException(msg);
}
public override string GetErrorHeader(RecognitionException e)
{
    return ErrorFormatter.GetErrorHeader(input.SourceName, e);
}
}


@lexer::members {
System.Collections.Generic.List<IToken> tokens = new System.Collections.Generic.List<IToken>();
public override void Emit(IToken tok) {
        state.token = tok;
       	tokens.Add(tok);
}
public override IToken NextToken() {
    	base.NextToken();
        if ( tokens.Count==0 ) {
            return Token.EOF_TOKEN;
        }
        IToken ret = tokens[0];
        tokens.Remove(ret);
        return ret;
}
/*
public override void ReportError(RecognitionException e) {
	base.ReportError(e);
        throw e;
}*/

public override void EmitErrorMessage(string msg)
{
    throw new asn1Parser.SyntaxErrorException(msg);
}
public override string GetErrorHeader(RecognitionException e)
{
    return ErrorFormatter.GetErrorHeader(input.SourceName, e);
}
}




/* ********************************************************************************************************************* */
/* *************************************    PARSER         ************************************************************* */
/* ********************************************************************************************************************* */

moduleDefinitions 
	:	moduleDefinition+	->^(ASN1_FILE moduleDefinition+)
	;	


definitiveIdentifier
	:	L_BRACKET definitiveObjIdComponent* R_BRACKET
	;

definitiveObjIdComponent
	:	identifier ( L_PAREN INT R_PAREN )?
	|	INT
	;	
	
moduleDefinition :  	a=modulereference	definitiveIdentifier?
			d=DEFINITIONS
			(EXPLICIT TAGS
			|IMPLICIT TAGS
			| AUTOMATIC TAGS)?
			(EXTENSIBILITY IMPLIED)?
			ASSIG_OP BEGIN
			exports?
			imports?
			(
				typeAssignment
				|valueAssignment
//				|valueSetAssignment
			)*
			END
			->  ^(MODULE_DEF[$d] modulereference EXPLICIT? IMPLICIT? AUTOMATIC? EXTENSIBILITY? exports? imports? typeAssignment* valueAssignment* /* valueSetAssignment* */)
			;

/*
EXPLICIT TAGS (default)==> all tags in explicit mode (i.e. multiple tags per type are encoded in BER)
IMPLICIT TAGS 	==>	all tags are implicit
AUTOMATIC TAGS	==> the components of all its structured types (SEQUENCE,SET or CHOICE) are automatically tagged by the compiler starting 
	from 0 by one-increment. By default, every component is tagged in the implicit mode except if it is a CHOICE type, an open type or a parameter 
		that is a type.

EXTENSIBILITY ==> An ENUMERATED, SEQUENCE, SET or CHOICE type that does not include an
	extension marker is extensible if the module includes the EXTENSIBILITY
	IMPLIED clause in its header
*/			
			
			
exports :
	   a=EXPORTS ALL ';' -> EXPORTS_ALL[$a]
	 | EXPORTS ((typereference | valuereference) (COMMA (typereference | valuereference))*)?  ';' 
	 	-> ^(EXPORTS typereference* valuereference*)?
;			

imports :
	IMPORTS importFromModule*  ';'	-> importFromModule*
	;
	
importFromModule
	:	(typereference | valuereference) (COMMA (typereference | valuereference))* FROM modulereference definitiveIdentifier?
		-> ^(IMPORTS_FROM_MODULE modulereference typereference* valuereference*  )
	;	
	
	
valueAssignment	
	:	valuereference type a=ASSIG_OP value	 -> ^(VAL_ASSIG[$a] valuereference type value)
	;		

/*		
valueSetAssignment
	:	typereference type '::=' L_BRACKET setOfValues R_BRACKET    -> ^(VAL_SET_ASSIG typereference type setOfValues)
	;		
*/	
typeAssignment 
	:	/*SPECIAL_COMMENT**/ typereference paramList? a=ASSIG_OP type -> ^(TYPE_ASSIG[$a] typereference type paramList?/*SPECIAL_COMMENT* */)
	;	

paramList 
	:
		'{' param (COMMA param)* '}'		-> ^(PARAM_LIST param*)
	;
	
param
	:	UID									->^(TYPE_PARAM	UID)
	|   type ':' LID						->^(VALUE_PARAM type LID)
	;
/* ********************************************************************************************************************* */
/* *************************************** TYPE DEFINITION ************************************************************* */
/* ********************************************************************************************************************* */

typeTag
	:	a='[' (t=UNIVERSAL | t=APPLICATION | t=PRIVATE)? (INT|val=valuereference)  ']' ( impOrExp=IMPLICIT | impOrExp=EXPLICIT)? 	
			-> ^(TYPE_TAG[$a] $t? INT? ^(VALUE_REFERENCE[(IToken)val.Start] $val)? $impOrExp?)
	;
	
/*
* In a given context, two tags are considered to be diferent if they are of diferent classes or if their respective numbers are diferent.
* If tagging class is missing (i.e. no UNIVERSAL, or  APPLICATION, or PRIVATE) are of context-specific class --> The tags make sense only inside the scope
	of the Father (father may be SEQUENCE, SET etc) --> they do not polute the global space
* UNIVERSAL   ==> used by people designing/extending ASN.1 
* APPLICATION ==> to tag types that would be referenced several times in a speci?cation
* PRIVATE ==> use of PRIVATE class tags is not recommended since 1994
* EXPLICIT (Default)==> In BER, more than one tags are encoded. Eg in Afters ::= CHOICE { cheese [0] IA5String, dessert [1] IA5String }
			the two tags [0] and [UNIVERSAL 22] are encoded if the alternative cheese is chosen
* IMPLICIT ==> Only one tag is encoded ; a tag marked IMPLICIT overwrites the tag that follows it (recursively)
*/		

/*
Note on constraints

(1) If Type is (literally) a SEQUENCE OF or SET OF type, the Constraint
applies on the type appearing after the keywords SEQUENCE OF or SET OF
FOR THIS REASON sequenceOfType AND setOfType ARE NOT FOLLOWED BY A constraint*
If Type is a reference to a SEQUENCE OF or SET OF type,
the Constraint applies to the SEQUENCE OF or SET OF type, and not to
the type that follows these keywords.		
(2) If several Constraints appear one after the other after Type, the possible
values for this type are those of the intersection of constraints .

*/
type	: typeTag?
(   nULL						-> ^(TYPE_DEF typeTag? nULL)
    |octetStringContainingType				-> ^(TYPE_DEF typeTag? octetStringContainingType )
    |bitStringType constraint*				-> ^(TYPE_DEF typeTag? bitStringType constraint* )
    |booleanType constraint*				-> ^(TYPE_DEF typeTag? booleanType constraint* )
    |enumeratedType constraint*				-> ^(TYPE_DEF typeTag? enumeratedType constraint* )
    |integerType constraint*				-> ^(TYPE_DEF typeTag? integerType constraint* )
    |realType constraint*		                -> ^(TYPE_DEF typeTag? realType constraint* )
    |stringType constraint*		                -> ^(TYPE_DEF typeTag? stringType constraint* )
    |referencedType	constraint*	                -> ^(TYPE_DEF typeTag? referencedType constraint* )
    |sequenceOfType 				        -> ^(TYPE_DEF typeTag? sequenceOfType)
    |choiceType	constraint*		        	-> ^(TYPE_DEF typeTag? choiceType constraint* )
    |sequenceType constraint*	                        -> ^(TYPE_DEF typeTag? sequenceType constraint* )
    |setType	constraint*		                -> ^(TYPE_DEF typeTag? setType )
    |setOfType						-> ^(TYPE_DEF typeTag? setOfType)
    |objectIdentifier constraint*	                -> ^(TYPE_DEF typeTag? objectIdentifier constraint* )
    |relativeOID	constraint*		        -> ^(TYPE_DEF typeTag? relativeOID constraint* )
    |selectionType					-> ^(TYPE_DEF typeTag? selectionType)
	|timeType								-> ^(TYPE_DEF typeTag? timeType)
)
;


	

selectionType	:	
	identifier a=LESS_THAN type								->^(SELECTION_TYPE[$a] identifier type)
;

sizeShortConstraint
	:	a=SIZE constraint									-> ^(SIZE_EXPR[$a] constraint)
	;

nULL:	NULL;

octetStringContainingType :
	: a=OCTET STRING L_PAREN CONTAINING type R_PAREN -> ^(CONTAINING type)
	;

bitStringType
	:	a=BIT STRING (L_BRACKET (bitStringItem (COMMA bitStringItem )* )? R_BRACKET )?	-> ^(BIT_STRING_TYPE[$a] bitStringItem*)
	;

bitStringItem 
	:	identifier a=L_PAREN (INT|valuereference) R_PAREN		->  ^(NUMBER_LST_ITEM[$a] identifier INT? valuereference?)
	;	
	
booleanType
	:	BOOLEAN
	;
	
enumeratedType 
	:	a=ENUMERATED L_BRACKET en1=enumeratedTypeItems  ( COMMA ext=EXT_MARK exceptionSpec? (COMMA en2=enumeratedTypeItems)? )? R_BRACKET
	-> ^(ENUMERATED_TYPE[$a] $en1 ($ext exceptionSpec? $en2?) ?)
	;
/*
enumeratedTypeItems2 
	:	 enumItemNoValue  (COMMA enumItemNoValue)* ->enumItemNoValue+
	|	 enumItemWithValue  (COMMA enumItemWithValue)* 		->enumItemWithValue+
	;		

enumItemNoValue
	:	identifier -> ^(NUMBER_LST_ITEM identifier )
	;

enumItemWithValue
	:	identifier  L_PAREN (INT|valuereference) R_PAREN -> ^(NUMBER_LST_ITEM identifier INT? valuereference?)
	;
*/

enumeratedTypeItems 
	:	 enumeratedLstItem (COMMA enumeratedLstItem)* ->enumeratedLstItem+
	;		


enumeratedLstItem	:	
	identifier ( L_PAREN (INT|val=valuereference) R_PAREN)? -> ^(NUMBER_LST_ITEM identifier INT? ^(VALUE_REFERENCE[(IToken)val.Start] $val)?)
;


integerType
	:	a=INTEGER ( L_BRACKET (integerTypeListItem (COMMA integerTypeListItem)*)? R_BRACKET)?	-> ^(INTEGER_TYPE[$a] integerTypeListItem*)
	;
	
integerTypeListItem 
	:	identifier a=L_PAREN (INT|val=valuereference) R_PAREN	-> ^(NUMBER_LST_ITEM[$a] identifier INT? ^(VALUE_REFERENCE[(IToken)val.Start] $val)?)
	;	
	
realType 
	:	REAL
	;
	
	
choiceType
	:	a=CHOICE L_BRACKET choiceItemsList choiceExtensionBody? R_BRACKET
	-> ^(CHOICE_TYPE[$a] choiceItemsList choiceExtensionBody?)
	;
	
choiceExtensionBody
	:	COMMA a=EXT_MARK exceptionSpec?  choiceListExtension?   (COMMA extMark2=EXT_MARK)?  
		-> ^(CHOICE_EXT_BODY[$a] exceptionSpec? choiceListExtension? $extMark2?)
	;	

choiceItemsList
	:	choiceItem (COMMA choiceItem)*	-> choiceItem+
	;
	
choiceItem
	:	identifier type				->  ^(CHOICE_ITEM identifier type )
	;	

choiceListExtension
	:	COMMA extensionAdditionAlternative (COMMA extensionAdditionAlternative)*	->	extensionAdditionAlternative+
	;	
	
extensionAdditionAlternative
	:	 a='[[' versionNumber? choiceItemsList ']]'	-> ^(CHOICE_EXT_ITEM[$a] versionNumber? choiceItemsList)
		| choiceItem
	;	

sequenceType
	:	a=SEQUENCE L_BRACKET sequenceOrSetBody?  R_BRACKET -> ^(SEQUENCE_TYPE[$a] sequenceOrSetBody?)
	;
	
setType	:	a=SET L_BRACKET sequenceOrSetBody?  R_BRACKET -> ^(SET_TYPE[$a] sequenceOrSetBody?)
	;	
	
sequenceOrSetBody	:
		  componentTypeList ( COMMA seqOrSetExtBody)?		-> ^(SEQUENCE_BODY componentTypeList seqOrSetExtBody?)
		| seqOrSetExtBody								-> ^(SEQUENCE_BODY seqOrSetExtBody)
	;
	
seqOrSetExtBody
	:	a=EXT_MARK exceptionSpec? (COMMA extensionAdditionList)? (COMMA extMark2=EXT_MARK   (COMMA componentTypeList )? )?
	->	^(SEQUENCE_EXT_BODY[$a] exceptionSpec? extensionAdditionList? $extMark2? componentTypeList?)
	;	
	
extensionAdditionList
	:	extensionAddition (COMMA extensionAddition)*		-> extensionAddition+
	;	

extensionAddition
	:	componentType
	       |extensionAdditionGroup
	;
extensionAdditionGroup
	:	a='[[' versionNumber? componentTypeList ']]'	-> ^(SEQUENCE_EXT_GROUP[$a] versionNumber? componentTypeList)
	;

componentTypeList 
	:	componentType  (COMMA componentType )*		-> componentType+
	;
	
componentType
	:	 identifier type (optOrDef=OPTIONAL | optOrDef=DEFAULT value)?	-> ^(SEQUENCE_ITEM identifier type $optOrDef? ^(DEFAULT_VALUE value)? )
		| a=COMPONENTS OF referencedType											-> ^(COMPONENTS_OF[$a] referencedType)
	;	
	
sequenceOfType
	:	a=SEQUENCE (sizeShortConstraint | constraint)? OF (identifier)? type			
		-> ^(SEQUENCE_OF_TYPE[$a] sizeShortConstraint? constraint? identifier? type )
	;
	
setOfType
	:	a=SET (sizeShortConstraint | constraint)? OF (identifier)? type				-> ^(SET_OF_TYPE[$a] sizeShortConstraint? constraint? identifier? type )
	;		

	
stringType	:
	 a=OCTET STRING	->	OCTECT_STING[$a]
	|NumericString
	|PrintableString
	|VisibleString
	|IA5String
	|TeletexString
	|VideotexString
	|GraphicString
	|GeneralString
	|UniversalString
	|BMPString
	|UTF8String
	|GeneralizedTime
	|UTCTime
	;
	
timeType : a=TIME L_PAREN SETTINGS StringLiteral R_PAREN  -> ^(TIME_TYPE[$a] StringLiteral)
	;

referencedType
	:	str1=UID ('.' str2=UID)?						 -> ^(REFERENCED_TYPE[$str1] $str1 $str2? )
	|	str1=UID ('.' str2=UID)?	actualParameterList  -> ^(PREFERENCED_TYPE[$str1] $str1 $str2? actualParameterList?)
	;	


actualParameterList 
	: '{' actualParameter (COMMA actualParameter)* '}'		-> ^(ACTUAL_PARAM_LIST actualParameter+)
	;
	
actualParameter 
	:  type			-> ^(ACTUAL_TYPE_PARAM type)
	| value			-> ^(ACTUAL_VALUE_PARAM value)
	;	
/* ********************************************************************************************************************* */
/* *************************************** VALUES DEFINITION *********************************************************** */
/* ********************************************************************************************************************* */

choiceValue :	identifier a=':' value	->^(CHOICE_VALUE[$a] identifier value)
	;	

namedValue 	:	a=identifier value		->^(NAMED_VALUE[(IToken)a.Start] identifier value)
	;
	
/*
To resolve another grammar Ambiguouity, we no more declare bitStringValue. The parser will only return valueList. Extra checking 
must be done during semantic checking.
bitStringValue
	:	L_BRACKET identifier (COMMA identifier)* R_BRACKET		->^(BIT_STRING_VALUE identifier+)
	;
*/	


/*
Grammar is ambiguous. For example
{ id 10} can be both objectIdentifierValue and namedValueList. The only way to resolve it is during Semantic checking. This parser
will always return objectIdentifierValue for such input
The same applies for {id}. It can be both objectIdentifierValue or valueList. It will be resolved during Semantic checking.
*/
value	:
		BitStringLiteral
	|	OctectStringLiteral
	|	TRUE
	|	FALSE
	|	StringLiteral
	|	NULL
	|	PLUS_INFINITY
	|   MINUS_INFINITY
	|	val=valuereference										->^(VALUE_REFERENCE[(IToken)val.Start] $val)
	|   INT
	| 	FloatingPointLiteral
	| a=L_BRACKET R_BRACKET 									-> ^(EMPTY_LIST[$a] )
	| a=L_BRACKET MANTISSA mant=INT COMMA BASE bas=INT COMMA EXPONENT exp=INT R_BRACKET -> ^(NUMERIC_VALUE2[$a] $mant $bas $exp)
	|   choiceValue
	| a=L_BRACKET namedValue (COMMA namedValue)* R_BRACKET 	->^(NAMED_VALUE_LIST[$a] namedValue+)
	| a=L_BRACKET value (COMMA value)* R_BRACKET			->^(VALUE_LIST[$a] value+)
	| a=L_BRACKET objectIdentifierComponent+ R_BRACKET		->^(OBJECT_ID_VALUE[$a] objectIdentifierComponent+ )
	;	
	
	
/*
a CharacterStringValue is
	a string literal
tuple		:   L_BRACKET INT COMMA INT R_BRACKET	;
*/	

objectIdentifierComponent
	:	identifier ( L_PAREN (INT | definedValue) R_PAREN )?	->^(OBJ_LST_ITEM1 identifier INT? definedValue?) //3 cases identifier or valuereference or identifier(number)
	| 	a=INT											->^(OBJ_LST_ITEM2[$a] INT)
	|	modulereference '.' valuereference			->^(DEFINED_VALUE modulereference valuereference)
	;


definedValue
	:	valuereference							-> ^(DEFINED_VALUE valuereference)
		| modulereference '.' valuereference	-> ^(DEFINED_VALUE modulereference valuereference)
	;

/* ********************************************************************************************************************* */
/* *************************************** Constraints DEFINITION ****************************************************** */
/* ********************************************************************************************************************* */
constraint 
	:	L_PAREN! unionConstraint (COMMA! EXT_MARK^ (COMMA! unionConstraint)?  exceptionSpec?)? R_PAREN!			
	;

exceptionSpec 
	:	'!' 
	(
		 INT											-> ^(EXCEPTION_SPEC  ^(EXCEPTION_SPEC_CONST_INT INT))
		|val=valuereference								-> ^(EXCEPTION_SPEC  ^(EXCEPTION_SPEC_VAL_REF ^(VALUE_REFERENCE[(IToken)val.Start] $val)))
		|type ':' value									-> ^(EXCEPTION_SPEC  ^(EXCEPTION_SPEC_TYPE_VALUE type value))
	)
														
	;


unionConstraint
	:	intersectionConstraint (UnionMark^ intersectionConstraint)*			
	|	ALL EXCEPT constraintExpression									->	^(ALL_EXCEPT constraintExpression)
	;	
	
//	UNION_ELEMENT2 is synonymous to INTERSECTION_SET
intersectionConstraint
	:	exceptConstraint (IntersectionMark^ exceptConstraint)*	
	;	
	
exceptConstraint 
	:	ex1=constraintExpression (EXCEPT^ ex2=constraintExpression)?			
	;	

//The grammar is unambigues since (NULL) can be interpreted as both single value constraint and Type inclusion constraint

constraintExpression
	: valueRangeExpression		//single value & range constraint. Single value can be applied to any type.
								//Range constraint can be applied to INTEGER and REAL
	| subtypeExpression      	//inclusion constraint, it can be applied to any type except CHARACTER STRING
	| sizeExpression			//applicable to BIT, OCTET, CHARACTER STRING and SEQUENCE OF, SET OF
								//The constraint that appears within a SIZE constraint must be a subset of INTEGER(0..MAX)
								//In other words, the constraint inside size can contain unions of single integer values, integer ranges
	| permittedAlphabetExpression
	| innerTypeExpression
	| patternExpression
	| L_PAREN unionConstraint R_PAREN	->	unionConstraint
	;	
	
valueRangeExpression
	: minVal=lowerEndValue  ((minIncl='<')? DOUBLE_DOT (maxIncl='<')? maxVal=upperEndValue)?			
		-> ^(VALUE_RANGE_EXPR $minVal ^(MAX_VAL_PRESENT $maxVal)?  ^(MIN_VAL_INCLUDED[$minIncl] $minIncl)? ^(MAX_VAL_INCLUDED[$maxIncl] $maxIncl) ? )
	;

lowerEndValue
	:	value
	| MIN
	;

upperEndValue 
	:	value
	| MAX
	;		


subtypeExpression: bInlc=INCLUDES? referencedType		-> ^(SUBTYPE_EXPR referencedType $bInlc?)
	;

sizeExpression : a=SIZE constraint			-> ^(SIZE_EXPR[$a] constraint)
	;
	
permittedAlphabetExpression : a=FROM constraint	-> ^(PERMITTED_ALPHABET_EXPR[$a] constraint)
	;

innerTypeExpression 
	: a=WITH COMPONENT constraint											-> ^(WITH_COMPONENT_CONSTR[$a] constraint)
	| a=WITH COMPONENTS L_BRACKET
			( EXT_MARK COMMA)?
			namedConstraintExpression  (COMMA namedConstraintExpression)* 
		R_BRACKET
		-> ^(WITH_COMPONENTS_CONSTR[$a] EXT_MARK? namedConstraintExpression+)
	;

namedConstraintExpression
	:	identifier  (constraint)? (eNum=PRESENT|eNum=ABSENT | eNum=OPTIONAL)?		
						-> ^(NAME_CONSTRAINT_EXPR identifier ^(INNER_CONSTRAINT constraint)? ^(EXT_MARK $eNum)?)
	;	

patternExpression : a=PATTERN value		-> ^(PATTERN_EXPR[$a] value)
	;
	


lID	:	LID;
modulereference	:	UID;
typereference	:	UID;
valuereference 	:	LID;		
identifier	:	LID;
versionNumber	:	INT ':'	->	INT;
objectIdentifier	:	a=OBJECT IDENTIFIER	->OBJECT_TYPE[$a];
relativeOID	:	RELATIVE_OID;
//signedNumber	:	(s='+'|s='-')? INT			->^(NUMERIC_VALUE INT $s?)
//	;


/* INFORMATION OBJECT CLASSES*/





/* ***************************************************************************************************************** */
/* ***************************************************************************************************************** */
/* **************************************      LEXER    ************************************************************ */
/* ***************************************************************************************************************** */
/* ***************************************************************************************************************** */

PLUS_INFINITY : 'PLUS-INFINITY';
MINUS_INFINITY	: 'MINUS-INFINITY';		
GeneralizedTime  :	'GeneralizedTime';
UTCTime  :	'UTCTime';
MANTISSA	: 'mantissa';
BASE		: 'base';
EXPONENT	: 'exponent';
UnionMark  :  '|'|'UNION';
IntersectionMark  :	'^' | 'INTERSECTION';
DEFINITIONS :	 'DEFINITIONS';
EXPLICIT:	 'EXPLICIT';
TAGS 	:	'TAGS';
IMPLICIT:	'IMPLICIT';
AUTOMATIC	:	'AUTOMATIC';
EXTENSIBILITY	:	'EXTENSIBILITY';
IMPLIED :	'IMPLIED';
BEGIN	:	'BEGIN';
END	:	'END';
EXPORTS	:	'EXPORTS';
ALL	: 	'ALL';
IMPORTS	:	'IMPORTS';
FROM	:	'FROM';
UNIVERSAL	: 'UNIVERSAL';
APPLICATION	: 'APPLICATION';
PRIVATE		:'PRIVATE';
BIT	: 'BIT';
STRING	:	'STRING';
BOOLEAN :	'BOOLEAN';
ENUMERATED	:'ENUMERATED';
INTEGER	:	'INTEGER';
TIME	:	'TIME';
SETTINGS :	'SETTINGS';
REAL	:	'REAL';
CHOICE	:	'CHOICE';
SEQUENCE	:'SEQUENCE';
OPTIONAL	:'OPTIONAL';
SIZE	:	'SIZE';
OF	:	'OF';
OCTET	:	'OCTET';
CONTAINING : 'CONTAINING';
MIN	: 	'MIN';
MAX	:	'MAX';
TRUE	:	'TRUE';
FALSE	:	'FALSE';
ABSENT	:	'ABSENT';
PRESENT	:	'PRESENT';
WITH 	:	'WITH';
COMPONENT	: 'COMPONENT';		
COMPONENTS 	: 'COMPONENTS';
DEFAULT 	: 'DEFAULT';
NULL		:'NULL';
PATTERN		:'PATTERN';
OBJECT 		:'OBJECT';
IDENTIFIER	:'IDENTIFIER';
RELATIVE_OID	:'RELATIVE-OID';
NumericString	:'NumericString';
PrintableString	:'PrintableString';
VisibleString	:'VisibleString';
IA5String	:'IA5String';
TeletexString	:'TeletexString';
VideotexString	:'VideotexString';
GraphicString	:'GraphicString';
GeneralString	:'GeneralString';
UniversalString	:'UniversalString';
BMPString	:'BMPString';
UTF8String	:'UTF8String';
INCLUDES	:'INCLUDES';
EXCEPT		:'EXCEPT';
SET		:'SET';
ASSIG_OP		: '::=';
L_BRACKET	:	'{';	
R_BRACKET	:	'}';	
L_PAREN		:	'(';
R_PAREN		:	')';
COMMA		:	',';

EXT_MARK	: '...';
DOUBLE_DOT  : '..';


LESS_THAN			: '<';


BitStringLiteral	:
	'\'' ('0'|'1'|' ' | '\t' | '\r' | '\n')* '\'B'
	;
	



OctectStringLiteral	:
	'\'' ('0'..'9'|'a'..'f'|'A'..'F'|' ' | '\t' | '\r' | '\n')* '\'H'
	;

StringLiteral 	: 	STR+ ;

fragment
STR 	:	'"' ( options {greedy=false;} : .)* '"' ;
			
UID  :   ('A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'-')* {if (Text.Contains("--"))
                {
                        MismatchedSetException mse = new MismatchedSetException(null, input);
                        Recover(mse);
                        throw mse;
                }}
    ;

LID  :   ('a'..'z') ('a'..'z'|'A'..'Z'|'0'..'9'|'-')* {if (Text.Contains("--"))
                {
                        MismatchedSetException mse = new MismatchedSetException(null, input);
                        Recover(mse);
                        throw mse;
                }}
    ;


fragment	
INT	:	('+'|'-')? ( '0' | ('1'..'9') ('0'..'9')*);


	
fragment
DIGITS
	:	('0'..'9')+;

FloatingPointLiteral
    :
    d=INT r=DOUBLE_DOT
    	  {
    	  $d.Type = INT;
    	  Emit($d);$d.Line = input.Line;
          $r.Type = DOUBLE_DOT;
    	  Emit($r);$r.Line = input.Line;
    	  }
    |	  INT DOT (DIGITS)? (Exponent)? 
    | 	d = INT { $d.Type = INT; Emit($d);$d.Line = input.Line;}
    ;

	
	
DOT	:	 '.';	





fragment
Exponent : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;


WS  :   (' ' | '\t' | '\r' | '\n')+ {$channel=HIDDEN;}
    ;    

COMMENT
    :   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;
/*
COMMENT2
    :   '--' ( options {greedy=false;} : . )* ('--'|'\r'?'\n') {$channel=HIDDEN;}
    ;
*/
COMMENT2
    : '--' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
    ;




