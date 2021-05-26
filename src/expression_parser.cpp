#include "expression_parser.h"

#include <sstream>
#include <unordered_set>
#include <jinja2cpp/template_env.h>

namespace jinja2
{

extern bool IsAFilterName(const std::string &filterName);
extern bool IsACallName(const std::string &filterName);

bool do_expr_debug = false;

static const char *fix_names[] = {
  "os",
  "os.environ",
  "os.environ.get",
  NULL
};

static bool is_func_name(const std::string &name)
{
    const char **h = fix_names;
    while (*h != NULL)
    {
        if (name == *h)
          return true;
        ++h;
    }
    return false;
}

template<typename T>
auto ReplaceErrorIfPossible(T& result, const Token& pivotTok, ErrorCode newError)
{
    auto& error = result.error();
    if (error.errorToken.range.startOffset == pivotTok.range.startOffset)
        return MakeParseError(newError, pivotTok);

    return result.get_unexpected();
}

ExpressionParser::ExpressionParser(const Settings& /* settings */, TemplateEnv* /* env */)
{

}

ExpressionParser::ParseResult<RendererPtr> ExpressionParser::Parse(LexScanner& lexer)
{
    auto evaluator = ParseFullExpression(lexer);
    if (!evaluator)
        return evaluator.get_unexpected();

    const auto &tok = lexer.NextToken();
    if (tok != Token::Eof)
    {
        if (do_expr_debug) std::cerr << " EOF ExpressionParser::Parse()" << std::endl;
        auto tok1 = tok;
        tok1.type = Token::Eof;

        return MakeParseError(ErrorCode::ExpectedToken, tok, {tok1});
    }

    RendererPtr result = std::make_shared<ExpressionRenderer>(*evaluator);

    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<FullExpressionEvaluator>> ExpressionParser::ParseFullExpression(LexScanner &lexer, bool includeIfPart)
{
    ExpressionEvaluatorPtr<FullExpressionEvaluator> result;
    LexScanner::StateSaver saver(lexer);

    ExpressionEvaluatorPtr<FullExpressionEvaluator> evaluator = std::make_shared<FullExpressionEvaluator>();
    auto value = ParseLogicalOr(lexer);
    if (!value)
    {
        if (do_expr_debug) std::cerr << "leave parse full expression unexprected" << std::endl; 
        return value.get_unexpected();
    }

    evaluator->SetExpression(*value);

    if (includeIfPart && lexer.EatIfEqual(Keyword::If))
    {
        auto ifExpr = ParseIfExpression(lexer);
        if (!ifExpr)
        {
            if (do_expr_debug) std::cerr << "leave parse full expression if part unexprected" << std::endl; 
            return ifExpr.get_unexpected();
        }
        evaluator->SetTester(*ifExpr);
    }

    saver.Commit();
    return evaluator;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseShift(LexScanner& lexer)
{
    auto left = ParseMathPlusMinus(lexer); // ParseStringConcat(lexer);
    auto tok = lexer.PeekNextToken();
    if (!left || (tok != Token::ShiftLeft && tok != Token::ShiftRight))
       return left;
    ExpressionEvaluatorPtr<Expression> result;
    result = *left;
    do {
        lexer.NextToken();
        auto right = ParseMathPlusMinus(lexer); // ParseStringConcat(lexer);
        if (!right)
        {
            if (do_expr_debug) std::cerr << "ParseShift return empty ... ignore left hand" << std::endl;
            return right.get_unexpected();
        }
        result = std::make_shared<BinaryExpression>(
            tok.type == Token::ShiftLeft ? BinaryExpression::BinaryShl : BinaryExpression::BinaryShr,
            result, *right);
        tok = lexer.PeekNextToken();
    } while (tok.type == Token::ShiftLeft || tok.type == Token::ShiftRight);
    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseBinaryAnd(LexScanner& lexer)
{
    auto left = ParseShift(lexer);
    auto tok = lexer.PeekNextToken();
    if (!left || tok != '&')
       return left;
    ExpressionEvaluatorPtr<Expression> result;
    lexer.NextToken();
    auto right = ParseBinaryAnd(lexer);
    if (!right)
    {
        if (do_expr_debug) std::cerr << "ParseBAnd return empty ... ignore left hand" << std::endl;
        return right.get_unexpected();
    }
    result = std::make_shared<BinaryExpression>(BinaryExpression::BinaryAnd, *left, *right);
    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseBinaryXor(LexScanner& lexer)
{
    auto left = ParseBinaryAnd(lexer);
    auto tok = lexer.PeekNextToken();
    if (!left || tok != '^')
       return left;
    lexer.NextToken();
    auto right = ParseBinaryXor(lexer);
    if (!right)
    {
        if (do_expr_debug) std::cerr << "ParseBXor return empty ... ignore left hand" << std::endl;
        return right.get_unexpected();
    }
    return std::make_shared<BinaryExpression>(BinaryExpression::BinaryXor, *left, *right);
}    

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseBinaryOr(LexScanner& lexer)
{
    auto left = ParseBinaryXor(lexer);
    auto tok = lexer.PeekNextToken();
    if (!left || tok != '|')
       return left;
    ExpressionEvaluatorPtr<Expression> result;
    result = *left;
    do {
        lexer.NextToken();
        tok = lexer.PeekNextToken();
        if (tok.type == Token::Identifier && IsAFilterName(AsString(tok.value)))
        {
            auto filter = ParseFilterExpression(lexer, '|', result);
            if (!filter)
                return filter.get_unexpected();
            result = std::make_shared<FilteredExpression>(std::move(result), *filter);
            tok = lexer.PeekNextToken();
            if (tok == '[' )
            {
                // slicing or subscription
                ParseResult<ExpressionEvaluatorPtr<Expression>> r1 = result;
                result = *ParseSubscript(lexer, *r1);
                tok = lexer.PeekNextToken();
                if (do_expr_debug) std::cerr << "parsed subscription I" << std::endl;
            }
        }
        else
        {
            auto right = ParseBinaryXor(lexer);
            if (!right)
            {
                if (do_expr_debug) std::cerr << "ParseBOr return empty ... ignore left hand" << std::endl;
                return right.get_unexpected();
            }
            result = std::make_shared<BinaryExpression>(BinaryExpression::BinaryOr, result, *right);
            tok = lexer.PeekNextToken();
        }
    } while (tok == '|');
    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseLogicalOr(LexScanner& lexer)
{
    auto left = ParseLogicalAnd(lexer);

    if (left && (lexer.EatIfEqual(Token::LogicalOr) || lexer.EatIfEqual(Keyword::LogicalOr)))
    {
        auto right = ParseLogicalOr(lexer);
        if (!right)
        {
            if (do_expr_debug) std::cerr << "ParseLogicalOr return empty ... ignore left hand" << std::endl;
            return right.get_unexpected();
        }

        return std::make_shared<BinaryExpression>(BinaryExpression::LogicalOr, *left, *right);
    }
    return left;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseLogicalAnd(LexScanner& lexer)
{
    auto left = ParseLogicalCompare(lexer);

    if (left && (lexer.EatIfEqual(Token::LogicalAnd) || lexer.EatIfEqual(Keyword::LogicalAnd)))
    {
        auto right = ParseLogicalAnd(lexer);
        if (!right)
        {
            if (do_expr_debug) std::cerr << "ParseLOr return empty ... ignore left hand" << std::endl;
            return right;
        }

        return std::make_shared<BinaryExpression>(BinaryExpression::LogicalAnd, *left, *right);
    }
    return left;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseLogicalCompare(LexScanner& lexer)
{
    auto left = ParseBinaryOr(lexer);
    if (!left)
        return left;

    const auto &tok = lexer.PeekNextToken();
    BinaryExpression::Operation operation;
    switch (tok.type)
    {
    case Token::Equal:
        operation = BinaryExpression::LogicalEq;
        break;
    case Token::NotEqual:
        operation = BinaryExpression::LogicalNe;
        break;
    case '<':
        operation = BinaryExpression::LogicalLt;
        break;
    case '>':
        operation = BinaryExpression::LogicalGt;
        break;
    case Token::GreaterEqual:
        operation = BinaryExpression::LogicalGe;
        break;
    case Token::LessEqual:
        operation = BinaryExpression::LogicalLe;
        break;
    default:
        switch (lexer.GetAsKeyword(tok))
        {
        case Keyword::LogicalNot:
            if (lexer.GetAsKeyword(lexer.PeekNextNextToken()) != Keyword::In)
                return left;
            lexer.NextToken();
            operation = BinaryExpression::NotIn;
            break;
        case Keyword::In:
            operation = BinaryExpression::In;
            break;
        case Keyword::Is:
        {
            bool is_not = false;
            lexer.NextToken();
            Token nextTok = lexer.NextToken();
            if (nextTok == Token::LogicalNot)
            {
                is_not = true;
                nextTok = lexer.NextToken();
            }
            if (nextTok != Token::Identifier)
                return MakeParseError(ErrorCode::ExpectedIdentifier, nextTok);
            std::string name = AsString(nextTok.value);
            if (name == "not")
            {
                is_not = true;
                nextTok = lexer.NextToken();
                if (nextTok != Token::Identifier)
                    return MakeParseError(ErrorCode::ExpectedIdentifier, nextTok);
                name = AsString(nextTok.value);
            }
            ParseResult<CallParamsInfo> params;

            if (lexer.EatIfEqual('('))
                params = ParseCallParams(lexer);
    
            if (name != "defined" && name != "undefined")
            {
                if (!params)
                    return params.get_unexpected();
            }
            ExpressionEvaluatorPtr<Expression> result = std::make_shared<IsExpression>(*left, std::move(name), std::move(*params));
            if (is_not)
                result = std::make_shared<UnaryExpression>(UnaryExpression::LogicalNot, result);

            return result;
        }
        default:
            return left;            
        }
    }
    lexer.NextToken();
    auto right = ParseBinaryOr(lexer);
    if (!right)
    {
        if (do_expr_debug) std::cerr << "ParseCmp return empty ... ignore left hand" << std::endl;
        return right;
    }
    return std::make_shared<BinaryExpression>(operation, *left, *right);
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseStringConcat(LexScanner& lexer)
{
    auto left = ParseMathPow(lexer);

    if (left && lexer.EatIfEqual('~'))
    {
        auto right = ParseStringConcat(lexer);
        if (!right)
        {
            if (do_expr_debug) std::cerr << "ParseStringConcat return empty ... ignore left hand" << std::endl;
            return right;
        }

        return std::make_shared<BinaryExpression>(BinaryExpression::StringConcat, *left, *right);
    }
    return left;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseMathPow(LexScanner& lexer)
{
    auto left = ParseUnaryPlusMinus(lexer);

    if (left && lexer.EatIfEqual(Token::MulMul))
    {
        auto right = ParseMathPlusMinus(lexer);
        if (!right)
        {
            if (do_expr_debug) std::cerr << "ParseMathPow right empty ... ignore left hand" << std::endl;
            return right;
        }

        return std::make_shared<BinaryExpression>(BinaryExpression::Pow, *left, *right);
    }
    return left;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseMathPlusMinus(LexScanner& lexer)
{
    auto left = ParseMathMulDiv(lexer);
    if (!left)
        return left;

    auto tok = lexer.PeekNextToken();
    BinaryExpression::Operation operation;
    switch (tok.type)
    {
    case '+':
        operation = BinaryExpression::Plus;
        break;
    case '-':
        operation = BinaryExpression::Minus;
        break;
    default:
        return left;
    }
    lexer.NextToken();
    auto right = ParseMathPlusMinus(lexer);
    if (!right)
    {
        if (do_expr_debug) std::cerr << "ParseMathPlusMinux return empty ... ignore left hand" << std::endl;
        return right;
    }
    return std::make_shared<BinaryExpression>(operation, *left, *right);
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseMathMulDiv(LexScanner& lexer)
{
    auto left = ParseStringConcat(lexer);
    if (!left)
        return left;

    const auto &tok = lexer.PeekNextToken();
    BinaryExpression::Operation operation;

    switch (tok.type)
    {
    case '*':
        operation = BinaryExpression::Mul;
        break;
    case '/':
        operation = BinaryExpression::Div;
        break;
    case Token::DivDiv:
        operation = BinaryExpression::DivInteger;
        break;
    case '%':
        // a hack ... might no longer be required
        if (lexer.PeekNextNextToken() == '}')
          return left;
        // see if we have here a fmt % (args)?
        if (lexer.PeekNextNextToken() == '(')
        {
            lexer.NextToken();
            auto filter = ParseFilterExpression(lexer, '%', left.value());
            if (!filter)
                return filter.get_unexpected();
            return std::make_shared<FilteredExpression>(std::move(*left), *filter);
        }
        operation = BinaryExpression::DivReminder;
        break;
    default:
        return left;
    }
    lexer.NextToken();
    auto right = ParseMathMulDiv(lexer);
    if (!right)
    {
        if (do_expr_debug) std::cerr << "ParseMathMulDiv return empty ... ignore left hand" << std::endl;
        return right;
    }

    return std::make_shared<BinaryExpression>(operation, *left, *right);
}

// u_expr ::= power | "-" u_expr | "+" u_expr
ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseUnaryPlusMinus(LexScanner& lexer)
{
    ExpressionEvaluatorPtr<Expression> result;
    const auto tok = lexer.PeekNextToken();
    if (tok == Token::Eof)
       return result;
    const auto isUnary = tok == '+' || tok == '-' || lexer.GetAsKeyword(tok) == Keyword::LogicalNot;
    if (isUnary)
        lexer.NextToken();
    // auto subExpr = !isUnary ? ParseValueExpression(lexer) : ParseUnaryPlusMinus(lexer);
    auto subExpr = ParseValueExpression(lexer);
    if (!subExpr)
        return subExpr;

    if (isUnary)
        result = std::make_shared<UnaryExpression>(tok == '+' ? UnaryExpression::UnaryPlus : (tok == '-' ? UnaryExpression::UnaryMinus : UnaryExpression::LogicalNot), *subExpr);
    else
        result = subExpr.value();

    while (lexer.PeekNextToken() == '|' || lexer.PeekNextToken() == Token::Dot)
    {
        Token tk2 = lexer.PeekNextNextToken();
        if (tk2.type == Token::Identifier && IsAFilterName(AsString(tk2.value)))
        {
            lexer.NextToken();
            auto filter = ParseFilterExpression(lexer, lexer.PeekNextToken() == '|' ? '|' : '.', result);
            if (!filter)
                return filter.get_unexpected();
            result = std::make_shared<FilteredExpression>(std::move(result), *filter);
            if ( lexer.PeekNextToken() == '[' )  {
                // slicing or subscription
                ParseResult<ExpressionEvaluatorPtr<Expression>> r1 = result;
                result = *ParseSubscript(lexer, *r1);
                if (do_expr_debug) std::cerr << "parsed subscription II" << std::endl;
            }
        }
        else
            break;
    }
    return result;
}

// literal:  identifier | stringliteral | bytesliteral || integer | floatnumber
ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseLiteral(LexScanner& lexer, Token::Type &typ)
{
    static const std::unordered_set<Keyword> forbiddenKw = {Keyword::Is, Keyword::In, Keyword::If, Keyword::Else };
    ParseResult<ExpressionEvaluatorPtr<Expression>> valueRef;
    Token tok = lexer.PeekNextToken();
    typ = tok.type;
    switch (tok.type)
    {
    case Token::Identifier:
        {
            auto kwType = lexer.GetAsKeyword(tok);
            if (forbiddenKw.count(kwType) != 0)
                return MakeParseError(ErrorCode::UnexpectedToken, tok);
            valueRef = std::make_shared<ValueRefExpression>(AsString(tok.value));
        }
        break;
    case Token::String:
    case Token::IntegerNum:
    case Token::FloatNum:
        valueRef = std::make_shared<ConstantExpression>(tok.value);
        break;
    case Token::None:
        valueRef = std::make_shared<ConstantExpression>(InternalValue(""));
        break;
    case Token::True:
        valueRef = std::make_shared<ConstantExpression>(InternalValue(true));
        break;
    case Token::False:
        valueRef = std::make_shared<ConstantExpression>(InternalValue(false));
        break;
    default:
        typ = Token::Eof;
        return valueRef;
    }
    lexer.NextToken();
    return valueRef;
}

// enclosure: "(" starred_expression ")" | list_display | dict_display | imagnumber
ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseEnclosure(LexScanner& lexer, Token::Type &typ)
{
    ParseResult<ExpressionEvaluatorPtr<Expression>> result;
    Token tok = lexer.PeekNextToken();
    typ = Token::String; // we want to allow direct subscripts/filters, etc
    switch (tok.type)
    {
    case '*':
    {
        lexer.NextToken();
        if (lexer.PeekNextToken() == Token::Eof)
          return MakeParseError(ErrorCode::UnexpectedToken, lexer.PeekNextToken());
        ParseResult<ExpressionEvaluatorPtr<Expression>> r = ParseBinaryOr(lexer);
        typ = Token::Eof;
        return r;
    }
    // "(" starred_expression ")"
    case '(':
        lexer.NextToken();
        result = ParseBracedExpressionOrTuple(lexer);
        break;
    // list_display
    case '[':
        lexer.NextToken();
        result = ParseTuple(lexer);
        break;
    // dict_display
    case '{':
        lexer.NextToken();
        result = ParseDictionary(lexer);
        break;
    // imagnumber - unsupported
    default:
        typ = Token::Eof;
        break;
    }
    return result;
}
// atom: indentifier | literal | enclosure
ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseAtomic(LexScanner& lexer, Token::Type &typ)
{
  ParseResult<ExpressionEvaluatorPtr<Expression>> result;
  typ = Token::Eof;
  result = ParseLiteral(lexer, typ);
  if (typ == Token::Eof)
    result = ParseEnclosure(lexer, typ);
  if (!result)
    return MakeParseError(ErrorCode::UnexpectedToken, lexer.PeekNextToken());
  return result;
}

// primary ::= atom | attributeref | subscription | sclicing | call
ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseValueExpression(LexScanner& lexer)
{
    std::string left = AsString(lexer.PeekNextToken().value);
    Token::Type typ = Token::Eof;
    ParseResult<ExpressionEvaluatorPtr<Expression>> valueRef = ParseAtomic(lexer, typ);

    if (valueRef && (typ == Token::String || typ == Token::Identifier))
    {
        do
        {
            auto tok = lexer.PeekNextToken();
            if (tok == '[' )
            {
                // slicing or subscription
                valueRef = ParseSubscript(lexer, *valueRef);
                if (do_expr_debug) std::cerr << "parsed subscription III" << std::endl;
            }
            else if (typ == Token::Identifier && lexer.EatIfEqual('('))
            {
                valueRef = ParseCall(lexer, *valueRef);
            }
            // attributeref
            else if ((typ == Token::Identifier || typ == Token::String) && tok == Token::Dot)
            {
                auto tk2 = lexer.PeekNextNextToken();
                if ( tk2.type == Token::Identifier)
                {
                    std::string right = AsString(tk2.value);
                    bool is_dotted_name = is_func_name(left + "." + right);
                    if (!is_dotted_name && IsAFilterName(right))
                    {
                        lexer.NextToken();
                        auto filter = ParseFilterExpression(lexer, '.', *valueRef);
                        if (!filter)
                        {
                            if (do_expr_debug) std::cerr << "filter ended unexpected " << AsString(tk2.value) << std::endl;
                            return filter.get_unexpected();
                        }
                        valueRef = std::make_shared<FilteredExpression>(std::move(*valueRef), *filter);
                        if (do_expr_debug) std::cerr << "filter added " << AsString(tk2.value) << std::endl;
                    }
                    else if (typ == Token::Identifier)
                    {
                        if (!is_dotted_name)
                        {
                            valueRef = ParseSubscriptDotName(lexer, *valueRef);
                            left = "";
                        }
                        else
                        {
                            left += ".";
                            left += right;
                            if (do_expr_debug)
                                std::cerr << "Token ," << left << "' read (" << right << ")" << std::endl;
                            valueRef = std::make_shared<ValueRefExpression>(left.c_str());
                            lexer.NextToken();
                            lexer.NextToken();
                        }
                    }
                    else
                        break;
                }
                else
                    break;
            }
            else
                break;
        } while (lexer.PeekNextToken().type == Token::Dot || lexer.PeekNextToken().type == '[' || lexer.PeekNextToken().type == '(');
    }
    return valueRef;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseBracedExpressionOrTuple(LexScanner& lexer)
{
    ExpressionEvaluatorPtr<Expression> result;
    bool isTuple = false;
    std::vector<ExpressionEvaluatorPtr<Expression>> exprs;

    for (;;)
    {
        Token pivotTok = lexer.PeekNextToken();
        auto expr = ParseFullExpression(lexer);

        if (!expr)
        {
            if (do_expr_debug) std::cerr << "within (...) failed"  << std::endl;
            return ReplaceErrorIfPossible(expr, pivotTok, ErrorCode::ExpectedRoundBracket);
        }

        exprs.push_back(*expr);
        Token tok = lexer.NextToken();
        if (tok == Token::Eof)
            return MakeParseError(ErrorCode::UnexpectedToken, tok);
        if (tok == ')')
            break;
        else if (tok == ',')
            isTuple = true;
    }
    if (isTuple)
        result = std::make_shared<TupleCreator>(std::move(exprs));
    else
        result = exprs[0];

    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseDictionary(LexScanner& lexer)
{
    ExpressionEvaluatorPtr<Expression> result;

    std::unordered_map<std::string, ExpressionEvaluatorPtr<Expression>> items;
    if (lexer.EatIfEqual('}'))
        return std::make_shared<DictCreator>(std::move(items));

    do
    {
        if (lexer.PeekNextToken() == '}' )
          break;
        Token key = lexer.NextToken();
        if (key != Token::String)
            return MakeParseError(ErrorCode::ExpectedStringLiteral, key);

        if (lexer.PeekNextToken() != '=' && lexer.PeekNextToken() != ':')
        {
            auto tok = lexer.PeekNextToken();
            auto tok1 = tok;
            tok1.type = Token::Assign;
            return MakeParseError(ErrorCode::ExpectedToken, tok, {tok1});
        }
        lexer.NextToken();
        auto pivotTok = lexer.PeekNextToken();
        auto expr = ParseFullExpression(lexer);
        if (!expr)
            return ReplaceErrorIfPossible(expr, pivotTok, ErrorCode::ExpectedExpression);

        items[AsString(key.value)] = *expr;

    } while (lexer.EatIfEqual(','));

    auto tok = lexer.NextToken();
    if (tok != '}')
        return MakeParseError(ErrorCode::ExpectedCurlyBracket, tok);

    result = std::make_shared<DictCreator>(std::move(items));

    return result;
}

// list_display ::=  "[" [starred_list | comprehension] "]"
// comprehension ::=  assignment_expression comp_for
// comp_for      ::=  ["async"] "for" target_list "in" or_test [comp_iter]
// comp_iter     ::=  comp_for | comp_if
// comp_if       ::=  "if" or_test [comp_iter]
ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseTuple(LexScanner& lexer)
{
    ExpressionEvaluatorPtr<Expression> result;

    std::vector<ExpressionEvaluatorPtr<Expression>> exprs;
    if (lexer.EatIfEqual(']'))
        return std::make_shared<TupleCreator>(exprs);

    do
    {
        auto expr = ParseFullExpression(lexer);
        if (!expr)
        {
            if ( lexer.PeekNextToken() == ']')
              break;
            return expr.get_unexpected();
        }

        exprs.push_back(*expr);
    } while (lexer.EatIfEqual(','));

    auto tok = lexer.NextToken();
    if (tok != ']')
        return MakeParseError(ErrorCode::ExpectedSquareBracket, tok);

    result = std::make_shared<TupleCreator>(std::move(exprs));

    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseCall(LexScanner& lexer, ExpressionEvaluatorPtr<Expression> valueRef)
{
    ExpressionEvaluatorPtr<Expression> result;

    ParseResult<CallParamsInfo> params = ParseCallParams(lexer);
    if (!params)
        return params.get_unexpected();

    result = std::make_shared<CallExpression>(valueRef, std::move(*params));

    return result;
}

ExpressionParser::ParseResult<CallParamsInfo> ExpressionParser::ParseCallParams(LexScanner& lexer, ExpressionEvaluatorPtr<Expression> &left)
{
    CallParamsInfo result;

    if (lexer.EatIfEqual(')'))
        return result;

    if (!left)
        return result;

    ExpressionEvaluatorPtr<FullExpressionEvaluator> ev = std::make_shared<FullExpressionEvaluator>();
    ev->SetExpression(left);
    result.posParams.push_back(ev);
    result.posParamsStarred.push_back(false);

    bool is_first = true;
    do
    {
        Token tok = lexer.PeekNextToken();
        std::string paramName;
        bool starred = false;
        if (tok == Token::Identifier && lexer.PeekNextNextToken() == '=')
        {
            lexer.NextToken();
            paramName = AsString(tok.value);
            lexer.EatToken();
        }
        // this will make out of an interatible list arguments
        if ( lexer.PeekNextToken() == '*')
        {
            lexer.NextToken();
            starred = true;
        }

        if ( lexer.PeekNextToken() == ')')
            break;
        if (!is_first)
        {
            auto valueExpr = ParseFullExpression(lexer);
            if (!valueExpr)
            {
                return valueExpr.get_unexpected();
            }
            else if (paramName.empty())
            {
                result.posParams.push_back(*valueExpr);
                result.posParamsStarred.push_back(starred);
            } else
                result.kwParams[paramName] = *valueExpr;
        }
        else 
        {
            left = *ParseLogicalOr(lexer);
            is_first = false;
        }
    } while (lexer.EatIfEqual(','));

    auto tok = lexer.NextToken();
    if (tok != ')')
        return MakeParseError(ErrorCode::ExpectedRoundBracket, tok);

    return result;
}

ExpressionParser::ParseResult<CallParamsInfo> ExpressionParser::ParseCallParams(LexScanner& lexer)
{
    CallParamsInfo result;

    if (lexer.EatIfEqual(')'))
        return result;

    do
    {
        Token tok = lexer.PeekNextToken();
        std::string paramName;
        bool starred = false;
        if (tok == Token::Identifier && lexer.PeekNextNextToken() == '=')
        {
            lexer.NextToken();
            paramName = AsString(tok.value);
            lexer.EatToken();
        }
        // this will make out of an interatible list arguments
        if ( lexer.PeekNextToken() == '*')
        {
            lexer.NextToken();
            starred = true;
        }

        if ( lexer.PeekNextToken() == ')')
            break;
        auto valueExpr = ParseFullExpression(lexer);
        if (!valueExpr)
        {
            return valueExpr.get_unexpected();
        }
        if (paramName.empty())
        {
            result.posParams.push_back(*valueExpr);
            result.posParamsStarred.push_back(starred);
        } else
            result.kwParams[paramName] = *valueExpr;

    } while (lexer.EatIfEqual(','));

    auto tok = lexer.NextToken();
    if (tok != ')')
        return MakeParseError(ErrorCode::ExpectedRoundBracket, tok);

    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseSubscriptDotName(LexScanner& lexer, ExpressionEvaluatorPtr<Expression> valueRef)
{
    ExpressionEvaluatorPtr<SubscriptExpression> result = std::make_shared<SubscriptExpression>(valueRef);
    Token tok = lexer.PeekNextToken();
    Token tk2 = lexer.PeekNextNextToken();
    if (tok != Token::Dot || tk2 != Token::Identifier)
       return MakeParseError(ErrorCode::ExpectedIdentifier, tok);;
    lexer.NextToken();
    lexer.NextToken();
    ParseResult<ExpressionEvaluatorPtr<Expression>> indexExpr;
    ParseResult<ExpressionEvaluatorPtr<Expression>> endExpr;
    ParseResult<ExpressionEvaluatorPtr<Expression>> sliceExpr;
    auto valueName = AsString(tk2.value);
    indexExpr = std::make_shared<ConstantExpression>(InternalValue(valueName));
    endExpr = std::make_shared<ConstantExpression>(InternalValue(valueName));
    sliceExpr = std::make_shared<ConstantExpression>(InternalValue(static_cast<int64_t>(1)));
    result->AddIndex(*indexExpr);
    result->AddIndexEnd(*endExpr);
    result->AddIndexSlice(*sliceExpr);
    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<Expression>> ExpressionParser::ParseSubscript(LexScanner& lexer, ExpressionEvaluatorPtr<Expression> valueRef)
{
    ExpressionEvaluatorPtr<SubscriptExpression> result = std::make_shared<SubscriptExpression>(valueRef);

    for (Token tok = lexer.PeekNextToken(); tok.type == '.' || tok.type == '['; tok = lexer.PeekNextToken())
    {
        ParseResult<ExpressionEvaluatorPtr<Expression>> indexExpr;
        ParseResult<ExpressionEvaluatorPtr<Expression>> endExpr;
        ParseResult<ExpressionEvaluatorPtr<Expression>> sliceExpr;
        if (tok == '.')
        {
            LexScanner::StateSaver sv(lexer);
            lexer.NextToken();
            tok = lexer.NextToken();
            if (tok.type != Token::Identifier)
            {
                sv.Commit();
                return MakeParseError(ErrorCode::ExpectedIdentifier, tok);
            }
            if (lexer.PeekNextToken().type == '(' || lexer.PeekNextToken().type == '.' 
                || IsAFilterName(AsString(tok.value)))
               break;
            if (do_expr_debug) std::cerr << " A named slice " << AsString(tok.value) << std::endl;
            sv.Commit();
            auto valueName = AsString(tok.value);
            indexExpr = std::make_shared<ConstantExpression>(InternalValue(valueName));
            endExpr = std::make_shared<ConstantExpression>(InternalValue(valueName));
            sliceExpr = std::make_shared<ConstantExpression>(InternalValue(static_cast<int64_t>(1)));
            if (do_expr_debug) std::cerr << "set for . index/end/slice" << std::endl;
        }
        else
        {
            lexer.NextToken();
            tok = lexer.PeekNextToken();
            if (tok != ':' && tok != ']')
            {
                auto expr = ParseFullExpression(lexer);

                if (!expr)
                    return expr.get_unexpected();
                else
                    indexExpr = *expr;
                tok = lexer.PeekNextToken();
            }
            else
            {
                indexExpr = std::make_shared<ConstantExpression>(InternalValue(static_cast<int64_t>(0)));
                ExpressionEvaluatorPtr<FullExpressionEvaluator> es = std::make_shared<FullExpressionEvaluator>();
                es->SetExpression(*indexExpr);
                indexExpr = es;
                if (do_expr_debug) std::cerr << "set indexExpr 0" << std::endl;
            }
            if (tok == ':')
            {
                lexer.NextToken();
                tok = lexer.PeekNextToken();

                if (tok != ':' && tok != ']')
                {
                    auto expr = ParseFullExpression(lexer);

                    if (!expr)
                        return expr.get_unexpected();
                    endExpr = *expr;
                    tok = lexer.PeekNextToken();

                }
                else
                {
                    endExpr = std::make_shared<ConstantExpression>(InternalValue(static_cast<int64_t>(-1)));
                    ExpressionEvaluatorPtr<FullExpressionEvaluator> es = std::make_shared<FullExpressionEvaluator>();
                    es->SetExpression(*endExpr);
                    endExpr = es;
                    if (do_expr_debug) std::cerr << "set endExpr -1" << std::endl;
                }
            }
            else
            {
                endExpr = std::make_shared<BinaryExpression>(BinaryExpression::Plus,
                                    *indexExpr,
                                    std::make_shared<ConstantExpression>(InternalValue(static_cast<int64_t>(1))));
                ExpressionEvaluatorPtr<FullExpressionEvaluator> es = std::make_shared<FullExpressionEvaluator>();
                es->SetExpression(*endExpr);
                endExpr = es;
                if (do_expr_debug) std::cerr << "set endExpr index + 1" << std::endl;
            }
            if (tok == ':')
            {
                lexer.NextToken();
                tok = lexer.PeekNextToken();
                if (tok != ':' && tok != ']')
                {
                    auto expr = ParseFullExpression(lexer);

                    if (!expr)
                        return expr.get_unexpected();
                    sliceExpr = *expr;
                    tok = lexer.PeekNextToken();
                }
                else
                {
                    sliceExpr = std::make_shared<ConstantExpression>(InternalValue(static_cast<int64_t>(1)));
                    ExpressionEvaluatorPtr<FullExpressionEvaluator> es = std::make_shared<FullExpressionEvaluator>();
                    es->SetExpression(*sliceExpr);
                    sliceExpr = es;
                    if (do_expr_debug) std::cerr << "set sliceExpr 1" << std::endl;
                }
            }
            else
            {
                sliceExpr = std::make_shared<ConstantExpression>(InternalValue(static_cast<int64_t>(1)));
                ExpressionEvaluatorPtr<FullExpressionEvaluator> es = std::make_shared<FullExpressionEvaluator>();
                es->SetExpression(*sliceExpr);
                sliceExpr = es;
                if (do_expr_debug) std::cerr << "set sliceExpr 1" << std::endl;
            }

            if (!lexer.EatIfEqual(']', &tok))
            {
                if (do_expr_debug) std::cerr << " squarebracket does not end as expected" << std::endl;
                return MakeParseError(ErrorCode::ExpectedSquareBracket, lexer.PeekNextToken());
            }
        }
        result->AddIndex(*indexExpr);
        result->AddIndexEnd(*endExpr);
        result->AddIndexSlice(*sliceExpr);
    }
    if (do_expr_debug) std::cerr << " return slice successful" << std::endl;
    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<ExpressionFilter>> ExpressionParser::ParseFilterExpression(LexScanner& lexer, int elem, ExpressionEvaluatorPtr<Expression> &left)
{
    ExpressionEvaluatorPtr<ExpressionFilter> result;

    auto startTok = lexer.PeekNextToken();
    try
    {
        do
        {
            Token tok;
            std::string name;
            if (elem != '%')
            {
                tok = lexer.NextToken();
                if (tok != Token::Identifier)
                    return MakeParseError(ErrorCode::ExpectedIdentifier, tok);
                name = AsString(tok.value);
            }
            else
            {
                name = std::string("cformat");
            }
            ParseResult<CallParamsInfo> params;

            if (lexer.PeekNextToken() == '(')
            {
                lexer.NextToken();
                // special handling of join if used with dot
                if (name == "join" && elem == '.')
                    params = ParseCallParams(lexer, left);
                else
                    params = ParseCallParams(lexer);
            }

            if (!params)
                return params.get_unexpected();

            auto filter = std::make_shared<ExpressionFilter>(name, std::move(*params));
            if (result)
            {
                filter->SetParentFilter(result);
                result = filter;
            }
            else
                result = filter;
            if ( elem == '%' || elem != (int) lexer.PeekNextToken().type )
              break;
            if (lexer.PeekNextNextToken().type != Token::Identifier
                 || !IsAFilterName(AsString(lexer.PeekNextNextToken().value)))
                break;
            lexer.NextToken();

        } while (1);
    }
    catch (const ParseError& error)
    {
        return nonstd::make_unexpected(error);
    }
    catch (const std::runtime_error&)
    {
        return MakeParseError(ErrorCode::UnexpectedException, startTok);
    }
    return result;
}

ExpressionParser::ParseResult<ExpressionEvaluatorPtr<IfExpression>> ExpressionParser::ParseIfExpression(LexScanner& lexer)
{
    ExpressionEvaluatorPtr<IfExpression> result;

    auto startTok = lexer.PeekNextToken();
    try
    {
        auto testExpr = ParseLogicalOr(lexer);
        if (!testExpr)
            return testExpr.get_unexpected();

        ParseResult<ExpressionEvaluatorPtr<>> altValue;
        if (lexer.GetAsKeyword(lexer.PeekNextToken()) == Keyword::Else)
        {
            lexer.EatToken();
            auto value = ParseFullExpression(lexer);
            if (!value)
                return value.get_unexpected();
            altValue = *value;
        }

        result = std::make_shared<IfExpression>(*testExpr, *altValue);
    }
    catch (const ParseError& error)
    {
        return nonstd::make_unexpected(error);
    }
    catch (const std::runtime_error& ex)
    {
        std::cout << "Filter parsing problem: " << ex.what() << std::endl;
        ExpressionEvaluatorPtr<IfExpression> result2;
        return result2;
    }
    return result;
}

} // jinja2
