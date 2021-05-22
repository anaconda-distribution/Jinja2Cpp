#ifndef EXPRESSION_EVALUATOR_H
#define EXPRESSION_EVALUATOR_H

#include "internal_value.h"
#include "render_context.h"

#include <memory>
#include <limits>

namespace jinja2
{

enum
{
    InvalidFn = -1,
    RangeFn = 1,
    LoopCycleFn = 2
};

class ExpressionEvaluatorBase;

template<typename T = ExpressionEvaluatorBase>
using ExpressionEvaluatorPtr = std::shared_ptr<T>;
using Expression = ExpressionEvaluatorBase;

class ExpressionEvaluatorBase
{
public:
    virtual ~ExpressionEvaluatorBase() {}

    virtual InternalValue Evaluate(RenderContext& values) = 0;
    virtual void Render(OutStream& stream, RenderContext& values);
};

struct CallParams
{
    std::unordered_map<std::string, InternalValue> kwParams;
    std::vector<InternalValue> posParams;
    std::vector<bool> posParamsStarred;
};

struct CallParamsInfo
{
    std::unordered_map<std::string, ExpressionEvaluatorPtr<>> kwParams;
    std::vector<ExpressionEvaluatorPtr<>> posParams;
    std::vector<bool> posParamsStarred;
};

struct ArgumentInfo
{
    std::string name;
    bool mandatory;
    InternalValue defaultVal;

    ArgumentInfo(std::string argName, bool isMandatory = false, InternalValue def = InternalValue())
        : name(std::move(argName))
        , mandatory(isMandatory)
        , defaultVal(std::move(def))
    {
    }
};

struct ParsedArgumentsInfo
{
    std::unordered_map<std::string, ExpressionEvaluatorPtr<>> args;
    std::unordered_map<std::string, ExpressionEvaluatorPtr<>> extraKwArgs;
    std::vector<ExpressionEvaluatorPtr<>> extraPosArgs;
    std::vector<bool> extraPosArgsStarred;

    ExpressionEvaluatorPtr<> operator[](const std::string& name) const
    {
        auto p = args.find(name);
        if (p == args.end())
            return ExpressionEvaluatorPtr<>();

        return p->second;
    }
};

struct ParsedArguments
{
    std::unordered_map<std::string, InternalValue> args;
    std::unordered_map<std::string, InternalValue> extraKwArgs;
    std::vector<InternalValue> extraPosArgs;
    std::vector<bool> extraPosArgsStarred;

    InternalValue operator[](const std::string& name) const
    {
        auto p = args.find(name);
        if (p == args.end())
            return InternalValue();

        return p->second;
    }
};

class ExpressionFilter;
class IfExpression;

class FullExpressionEvaluator : public ExpressionEvaluatorBase
{
public:
    void SetExpression(ExpressionEvaluatorPtr<Expression> expr)
    {
        m_expression = std::move(expr);
    }
    void SetTester(ExpressionEvaluatorPtr<IfExpression> expr)
    {
        m_tester = std::move(expr);
    }
    InternalValue Evaluate(RenderContext& values) override;
    void Render(OutStream &stream, RenderContext &values) override;
private:
    ExpressionEvaluatorPtr<Expression> m_expression;
    ExpressionEvaluatorPtr<IfExpression> m_tester;
};

class ValueRefExpression : public Expression
{
public:
    ValueRefExpression(std::string valueName)
        : m_valueName(valueName)
    {
    }
    InternalValue Evaluate(RenderContext& values) override;
private:
    std::string m_valueName;
};

class SubscriptExpression : public Expression
{
public:
    SubscriptExpression(ExpressionEvaluatorPtr<Expression> value)
        : m_value(value)
    {
    }
    InternalValue Evaluate(RenderContext& values) override;
    void AddIndex(ExpressionEvaluatorPtr<Expression> value)
    {
        m_subscriptExprs.push_back(value);
    }
    void AddIndexEnd(ExpressionEvaluatorPtr<Expression> value)
    {
        m_subscriptExprsEnd.push_back(value);
    }
    void AddIndexSlice(ExpressionEvaluatorPtr<Expression> value)
    {
        m_subscriptExprsSlice.push_back(value);
    }

private:
    ExpressionEvaluatorPtr<Expression> m_value;
    std::vector<ExpressionEvaluatorPtr<Expression>> m_subscriptExprs;
    std::vector<ExpressionEvaluatorPtr<Expression>> m_subscriptExprsEnd;
    std::vector<ExpressionEvaluatorPtr<Expression>> m_subscriptExprsSlice;
};

class FilteredExpression : public Expression
{
public:
    explicit FilteredExpression(ExpressionEvaluatorPtr<Expression> expression, ExpressionEvaluatorPtr<ExpressionFilter> filter)
        : m_expression(std::move(expression))
        , m_filter(std::move(filter))
    {
    }
    InternalValue Evaluate(RenderContext&) override;

private:
    ExpressionEvaluatorPtr<Expression> m_expression;
    ExpressionEvaluatorPtr<ExpressionFilter> m_filter;
};

class ConstantExpression : public Expression
{
public:
    ConstantExpression(InternalValue constant)
        : m_constant(constant)
    {}
    InternalValue Evaluate(RenderContext&) override
    {
        return m_constant;
    }
private:
    InternalValue m_constant;
};

class TupleCreator : public Expression
{
public:
    TupleCreator(std::vector<ExpressionEvaluatorPtr<>> exprs)
        : m_exprs(std::move(exprs))
    {
    }

    InternalValue Evaluate(RenderContext&) override;
private:
    std::vector<ExpressionEvaluatorPtr<>> m_exprs;
};
/*
class DictionaryCreator : public Expression
{
public:
    DictionaryCreator(std::unordered_map<std::string, ExpressionEvaluatorPtr<>> items)
        : m_items(std::move(items))
    {
    }

    InternalValue Evaluate(RenderContext&) override;

private:
    std::unordered_map<std::string, ExpressionEvaluatorPtr<>> m_items;
};*/

class DictCreator : public Expression
{
public:
    DictCreator(std::unordered_map<std::string, ExpressionEvaluatorPtr<>> exprs)
        : m_exprs(std::move(exprs))
    {
    }

    InternalValue Evaluate(RenderContext&) override;

private:
    std::unordered_map<std::string, ExpressionEvaluatorPtr<>> m_exprs;
};

class UnaryExpression : public Expression
{
public:
    enum Operation
    {
        LogicalNot,
        UnaryPlus,
        UnaryMinus
    };

    UnaryExpression(Operation oper, ExpressionEvaluatorPtr<> expr)
        : m_oper(oper)
        , m_expr(expr)
    {}
    InternalValue Evaluate(RenderContext&) override;
private:
    Operation m_oper;
    ExpressionEvaluatorPtr<> m_expr;
};

class IsExpression : public Expression
{
public:
    virtual ~IsExpression() {}

    struct ITester
    {
        virtual ~ITester() {}
        virtual bool Test(const InternalValue& baseVal, RenderContext& context) = 0;
    };

    using TesterFactoryFn = std::function<std::shared_ptr<ITester>(CallParamsInfo params)>;

    IsExpression(ExpressionEvaluatorPtr<> value, const std::string& tester, CallParamsInfo params);
    InternalValue Evaluate(RenderContext& context) override;

private:
    ExpressionEvaluatorPtr<> m_value;
    std::shared_ptr<ITester> m_tester;
};

class BinaryExpression : public Expression
{
public:
    enum Operation
    {
        LogicalAnd,
        LogicalOr,
        LogicalEq,
        LogicalNe,
        LogicalGt,
        LogicalLt,
        LogicalGe,
        LogicalLe,
        In,
        NotIn,
        BinaryOr,
        BinaryXor,
        BinaryAnd,
        BinaryShl,
        BinaryShr,
        Plus,
        Minus,
        Mul,
        Div,
        DivReminder,
        DivInteger,
        Pow,
        StringConcat
    };

    enum CompareType
    {
        Undefined = 0,
        CaseSensitive = 0,
        CaseInsensitive = 1
    };

    BinaryExpression(Operation oper, ExpressionEvaluatorPtr<> leftExpr, ExpressionEvaluatorPtr<> rightExpr);
    InternalValue Evaluate(RenderContext&) override;
private:
    Operation m_oper;
    ExpressionEvaluatorPtr<> m_leftExpr;
    ExpressionEvaluatorPtr<> m_rightExpr;
    std::shared_ptr<IsExpression::ITester> m_inTester;
};


class CallExpression : public Expression
{
public:
    virtual ~CallExpression() {}

    CallExpression(ExpressionEvaluatorPtr<> valueRef, CallParamsInfo params)
        : m_valueRef(std::move(valueRef))
        , m_params(std::move(params))
    {
    }

    InternalValue Evaluate(RenderContext &values) override;
    void Render(OutStream &stream, RenderContext &values) override;

    auto& GetValueRef() const {return m_valueRef;}
    auto& GetParams() const {return m_params;}

private:
    InternalValue CallArbitraryFn(RenderContext &values);
    InternalValue CallGlobalRange(RenderContext &values);
    InternalValue CallLoopCycle(RenderContext &values);

private:
    ExpressionEvaluatorPtr<> m_valueRef;
    CallParamsInfo m_params;
};

class ExpressionFilter
{
public:
    virtual ~ExpressionFilter() {}

    struct IExpressionFilter
    {
        virtual ~IExpressionFilter() {}
        virtual InternalValue Filter(const InternalValue& baseVal, RenderContext& context) = 0;
    };

    using FilterFactoryFn = std::function<std::shared_ptr<IExpressionFilter>(CallParamsInfo params)>;

    ExpressionFilter(const std::string& filterName, CallParamsInfo params);

    InternalValue Evaluate(const InternalValue& baseVal, RenderContext& context);
    void SetParentFilter(std::shared_ptr<ExpressionFilter> parentFilter)
    {
        m_parentFilter = std::move(parentFilter);
    }
private:
    std::shared_ptr<IExpressionFilter> m_filter;
    std::shared_ptr<ExpressionFilter> m_parentFilter;
};


class IfExpression
{
public:
    virtual ~IfExpression() {}

    IfExpression(ExpressionEvaluatorPtr<> testExpr, ExpressionEvaluatorPtr<> altValue)
        : m_testExpr(testExpr)
        , m_altValue(altValue)
    {
    }

    bool Evaluate(RenderContext& context);
    InternalValue EvaluateAltValue(RenderContext& context);

    void SetAltValue(ExpressionEvaluatorPtr<> altValue)
    {
        m_altValue = std::move(altValue);
    }

private:
    ExpressionEvaluatorPtr<> m_testExpr;
    ExpressionEvaluatorPtr<> m_altValue;
};

namespace helpers
{
ParsedArguments ParseCallParams(const std::initializer_list<ArgumentInfo>& argsInfo, const CallParams& params, bool& isSucceeded);
ParsedArguments ParseCallParams(const std::vector<ArgumentInfo>& args, const CallParams& params, bool& isSucceeded);
ParsedArgumentsInfo ParseCallParamsInfo(const std::initializer_list<ArgumentInfo>& argsInfo, const CallParamsInfo& params, bool& isSucceeded);
ParsedArgumentsInfo ParseCallParamsInfo(const std::vector<ArgumentInfo>& args, const CallParamsInfo& params, bool& isSucceeded);
CallParams EvaluateCallParams(const CallParamsInfo& info, RenderContext& context);
}
} // jinja2

#endif // EXPRESSION_EVALUATOR_H
