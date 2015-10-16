
#ifndef BASE_LOGICAL_FORMULA_H__
#define BASE_LOGICAL_FORMULA_H__

#include <istream>
#include <map>
#include <ostream>
#include <set>

#include "base/symbolic_expression.h"
#include "base/symbolic_predicate.h"

using std::istream;
using std::map;
using std::ostream;
using std::set;

namespace crest {
  
  class LogicalFormula : public SymbolicExpr {
  public:
    LogicalFormula(SymbolicPred* pred);
    LogicalFormula(binary_op_t op, SymbolicExpr* left, SymbolicExpr* right);
    LogicalFormula(binary_op_t op, LogicalFormula* left, LogicalFormula* right);
    LogicalFormula(const LogicalFormula& f);
    LogicalFormula();
    ~LogicalFormula();

    void Negate();
    bool IsConcrete() const;
    void AppendVars(set<var_t>* vars) const;
    bool DependsOn(const map<var_t,type_t>& vars) const;
    void AppendToString(string* s) const;
    void Serialize(string* s) const;
    bool Parse(istream& s);

    const SymbolicExpr& operator+=(const SymbolicExpr& e);
    const SymbolicExpr& operator-=(const SymbolicExpr& e);
    const SymbolicExpr& operator+=(value_t c);
    const SymbolicExpr& operator-=(value_t c);
    const SymbolicExpr& operator*=(value_t c);
    bool operator==(const SymbolicExpr& e) const;

    value_t const_term() const;
    const map<var_t,value_t>& terms() const;

    bool IsLogical() const { return true; }
    bool IsPredicate() const { return pred_ != NULL; }
    binary_op_t op() const { return op_; }
    const SymbolicPred& predicate() const { return *pred_; }
    const LogicalFormula& left() const { return *left_; }
    const LogicalFormula& right() const { return *right_; }

    string ToString() const;
  private:
    SymbolicPred* pred_;
    binary_op_t op_;
    LogicalFormula* left_;
    LogicalFormula* right_;
  };

}  // namespace crest

#endif  // BASE_LOGICAL_FORMULA_H__

