
#include <assert.h>
#include <istream>
#include <map>
#include <ostream>
#include <set>
#include <stdexcept>

#include "base/basic_types.h"
#include "base/logical_formula.h"

namespace crest {

  LogicalFormula::LogicalFormula() {
    pred_ = NULL;
    left_ = NULL;
    right_ = NULL;
  }  

  LogicalFormula::LogicalFormula(SymbolicPred* pred) {
    pred_ = pred;
    left_ = NULL;
    right_ = NULL;
  }  

  LogicalFormula::LogicalFormula(binary_op_t op, SymbolicExpr* left, SymbolicExpr* right) {
    pred_ = NULL;
    op_ = op;
    left_ = dynamic_cast<LogicalFormula*>(left);
    right_ = dynamic_cast<LogicalFormula*>(right);
    if (left_ == NULL || right_ == NULL)
      throw std::runtime_error("In the constructor of LogicalFormula: logical operators can only be applied to predicates.");
  }
  
  LogicalFormula::LogicalFormula(binary_op_t op, LogicalFormula* left, LogicalFormula* right) {
    pred_ = NULL;
    op_ = op;
    left_ = left;
    right_ = right;
  }

  LogicalFormula::LogicalFormula(const LogicalFormula& f) {
    pred_ = f.pred_ == NULL ? NULL : new SymbolicPred(*f.pred_);
    op_ = f.op_;
    left_ = f.left_ == NULL ? NULL : new LogicalFormula(*f.left_);
    right_ = f.right_ == NULL ? NULL : new LogicalFormula(*f.right_);
  }

  LogicalFormula::~LogicalFormula() {
    if (pred_) {
      delete pred_;
      pred_ = NULL;
    } else {
      delete left_;
      delete right_;
      left_ = NULL;
      right_ = NULL;
    }
  }

  void LogicalFormula::Negate() {
    if (pred_) {
      pred_->Negate();
    } else {
      if (op_ == ops::L_AND)
        op_ = ops::L_OR;
      else if (op_ == ops::L_OR)
        op_ = ops::L_AND;
      
      left_->Negate();
      right_->Negate();
    }
  }

  bool LogicalFormula::IsConcrete() const {
    return false;
  }

  void LogicalFormula::AppendVars(set<var_t>* vars) const {
    if (pred_) {
      pred_->AppendVars(vars);
    } else {
      left_->AppendVars(vars);
      right_->AppendVars(vars);
    }
  }

  bool LogicalFormula::DependsOn(const map<var_t,type_t>& vars) const {
    if (pred_)
      return pred_->DependsOn(vars);
    else
      return left_->DependsOn(vars) || right_->DependsOn(vars);
  }

  void LogicalFormula::AppendToString(string* s) const {
    if (pred_) {
      pred_->AppendToString(s);
    } else {
      s->append("(");
      if (op_ == ops::L_AND)
        s->append("and");
      else
        s->append("or");
      s->append(" (");
      left_->AppendToString(s);
      s->append(") (");
      right_->AppendToString(s);
      s->append("))");
    }
  }

  void LogicalFormula::Serialize(string* s) const {
    if (pred_) {
      s->push_back('p');
      pred_->Serialize(s);
    } else {
      s->push_back('f');
      s->push_back(static_cast<char>(op_));
      left_->Serialize(s);
      right_->Serialize(s);
    }
  }

  bool LogicalFormula::Parse(istream& s) {
    char type = static_cast<char>(s.get());
    if (type == 'p') {
      pred_ = new SymbolicPred();
      if (!pred_->Parse(s))
        return false;
    } else if (type == 'f') {
      op_ = static_cast<binary_op_t>(s.get());
      left_ = new LogicalFormula();
      right_ = new LogicalFormula();
      if (!left_->Parse(s))
        return false;
      if (!right_->Parse(s))
        return false;
    } else {
      assert(false);
    }

    return !s.fail();
  }

  const SymbolicExpr& LogicalFormula::operator+=(const SymbolicExpr& e) {
    throw std::runtime_error("Unsupported operation '+=' on logical formulas.");
  }
  
  const SymbolicExpr& LogicalFormula::operator-=(const SymbolicExpr& e) {
    throw std::runtime_error("Unsupported operation '-=' on logical formulas.");
  }

  const SymbolicExpr& LogicalFormula::operator+=(value_t c) {
    throw std::runtime_error("Unsupported operation '+=' on logical formulas.");
  }
  
  const SymbolicExpr& LogicalFormula::operator-=(value_t c) {
    throw std::runtime_error("Unsupported operation '-=' on logical formulas.");
  }

  const SymbolicExpr& LogicalFormula::operator*=(value_t c) {
    throw std::runtime_error("Unsupported operation '*=' on logical formulas.");
  }

  bool LogicalFormula::operator==(const SymbolicExpr& e) const {
    if (e.IsLogical()) {
      const LogicalFormula* f = dynamic_cast<const LogicalFormula*>(&e);
      if (IsPredicate())
        return f->IsPredicate() && pred_ == f->pred_;
      else
        return !f->IsPredicate() && op_ == f->op_ && left_ == f->left_ && right_ == f->right_;
    } else
      return false;
  }

  value_t LogicalFormula::const_term() const {
    throw std::runtime_error("Unsupported operation 'const_term()' on logical formulas.");
  }
  
  const map<var_t,value_t>& LogicalFormula::terms() const {
    throw std::runtime_error("Unsupported operation 'terms()' on logical formulas.");
  }

  string LogicalFormula::ToString() const {
    string s;
    AppendToString(&s);
    return s;
  }
}
