// Copyright (c) 2008, Jacob Burnim (jburnim@cs.berkeley.edu)
//
// This file is part of CREST, which is distributed under the revised
// BSD license.  A copy of this license can be found in the file LICENSE.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LICENSE
// for details.

#include "base/symbolic_path.h"

namespace crest {

SymbolicPath::SymbolicPath() { }

SymbolicPath::SymbolicPath(bool pre_allocate) {
  if (pre_allocate) {
    // To cut down on re-allocation.
    branches_.reserve(4000000);
    constraints_idx_.reserve(50000);
    constraints_.reserve(50000);
    assumptions_.reserve(1000);
    assumptions_idx_.reserve(1000);
  }
}

SymbolicPath::~SymbolicPath() {
  for (size_t i = 0; i < constraints_.size(); i++)
    delete constraints_[i];
  for (size_t i = 0; i < assumptions_.size(); i++)
    delete assumptions_[i];
}

void SymbolicPath::Swap(SymbolicPath& sp) {
  branches_.swap(sp.branches_);
  constraints_idx_.swap(sp.constraints_idx_);
  constraints_.swap(sp.constraints_);
  assumptions_.swap(sp.assumptions_);
  assumptions_idx_.swap(sp.assumptions_idx_);
}

void SymbolicPath::Push(branch_id_t bid) {
  branches_.push_back(bid);
}

void SymbolicPath::Push(branch_id_t bid, SymbolicPred* constraint) {
  if (constraint) {
    constraints_.push_back(constraint);
    constraints_idx_.push_back(branches_.size());
  }
  branches_.push_back(bid);
}

  void SymbolicPath::Assume(SymbolicPred* assumption) {
    if (assumption) {
      assumptions_.push_back(assumption);
      assumptions_idx_.push_back(constraints_.size());
    }
  }

  void SymbolicPath::assumptions(size_t id, vector<const SymbolicPred*>& vec) const {
    for (size_t i = 0; i < assumptions_idx_.size() && id >= assumptions_idx_[i] ; i++)
      vec.push_back(assumptions_[i]);
  }

void SymbolicPath::Serialize(string* s) const {
  typedef vector<SymbolicPred*>::const_iterator ConIt;
  
  // Write the path.
  size_t len = branches_.size();
  s->append((char*)&len, sizeof(len));
  s->append((char*)&branches_.front(), branches_.size() * sizeof(branch_id_t));

  // Write the path constraints.
  len = constraints_.size();
  s->append((char*)&len, sizeof(len));
  s->append((char*)&constraints_idx_.front(), constraints_.size() * sizeof(size_t));
  for (ConIt i = constraints_.begin(); i != constraints_.end(); ++i) {
    (*i)->Serialize(s);
  }
  
  // Write the assumptions.
  len = assumptions_.size();
  s->append((char*)&len, sizeof(len));
  s->append((char*)&assumptions_idx_.front(), assumptions_.size() * sizeof(size_t));  
  for (ConIt i = assumptions_.begin(); i != assumptions_.end(); ++i) {
    (*i)->Serialize(s);
  }

}

bool SymbolicPath::Parse(istream& s) {
  typedef vector<SymbolicPred*>::iterator ConIt;
  size_t len;

  // Read the path.
  s.read((char*)&len, sizeof(size_t));
  branches_.clear();
  branches_.resize(len);
  s.read((char*)&branches_.front(), len * sizeof(branch_id_t));
  if (s.fail())
    return false;

  // Clean up any existing path constraints.
  for (size_t i = 0; i < constraints_.size(); i++)
    delete constraints_[i];
  for (size_t i = 0; i < assumptions_.size(); i++)
    delete assumptions_[i];

  // Read the path constraints.
  s.read((char*)&len, sizeof(size_t));
  constraints_idx_.clear();
  constraints_.clear();
  constraints_idx_.resize(len);
  constraints_.resize(len);
  s.read((char*)&constraints_idx_.front(), len * sizeof(size_t));
  for (ConIt i = constraints_.begin(); i != constraints_.end(); ++i) {
    *i = new SymbolicPred();
    if (!(*i)->Parse(s))
      return false;
  }

  // Read the assumptions.
  s.read((char*)&len, sizeof(size_t));
  assumptions_idx_.clear();
  assumptions_.clear();
  assumptions_idx_.resize(len);
  assumptions_.resize(len);
  s.read((char*)&assumptions_idx_.front(), len * sizeof(size_t));
  for (ConIt i = assumptions_.begin(); i != assumptions_.end(); ++i) {
    *i = new SymbolicPred();
    if (!(*i)->Parse(s))
      return false;
  }

  return !s.fail();
}

}  // namespace crest
