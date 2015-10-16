// Copyright (c) 2008, Jacob Burnim (jburnim@cs.berkeley.edu)
//
// This file is part of CREST, which is distributed under the revised
// BSD license.  A copy of this license can be found in the file LICENSE.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LICENSE
// for details.

#ifndef BASE_SYMBOLIC_EXPRESSION_H__
#define BASE_SYMBOLIC_EXPRESSION_H__

#include <istream>
#include <map>
#include <ostream>
#include <set>
#include <string>

#include "base/basic_types.h"

using std::istream;
using std::map;
using std::ostream;
using std::set;
using std::string;

namespace crest {

class SymbolicExpr {
 public:
  // Constructs a symbolic expression for the constant 0.
  SymbolicExpr();

  // Constructs a symbolic expression for the given constant 'c'.
  explicit SymbolicExpr(value_t c);

  // Constructs a symbolic expression for the singleton 'c' * 'v'.
  SymbolicExpr(value_t c, var_t v);

  // Copy constructor.
  SymbolicExpr(const SymbolicExpr& e);

  // Desctructor.
  virtual ~SymbolicExpr();

  virtual void Negate();
  virtual bool IsConcrete() const { return coeff_.empty(); }
  size_t Size() const { return (1 + coeff_.size()); }
  virtual void AppendVars(set<var_t>* vars) const;
  virtual bool DependsOn(const map<var_t,type_t>& vars) const;

  virtual void AppendToString(string* s) const;

  virtual void Serialize(string* s) const;
  virtual bool Parse(istream& s);

  // Arithmetic operators.
  virtual const SymbolicExpr& operator+=(const SymbolicExpr& e);
  virtual const SymbolicExpr& operator-=(const SymbolicExpr& e);
  virtual const SymbolicExpr& operator+=(value_t c);
  virtual const SymbolicExpr& operator-=(value_t c);
  virtual const SymbolicExpr& operator*=(value_t c);
  virtual bool operator==(const SymbolicExpr& e) const;

  // Accessors.
  virtual value_t const_term() const { return const_; }
  virtual const map<var_t,value_t>& terms() const { return coeff_; }
  typedef map<var_t,value_t>::const_iterator TermIt;

  virtual bool IsLogical() const;
  string ToString() const;
 private:
  value_t const_;
  map<var_t,value_t> coeff_;
};

}  // namespace crest

#endif  // BASE_SYMBOLIC_EXPRESSION_H__
