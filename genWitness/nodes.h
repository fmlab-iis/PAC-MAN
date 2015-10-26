#ifndef __NODES_H__
#define __NODES_H__

#include <iostream>
#include <vector> 
#include <string>
#include <set>
class Node
{
public:
  Node() {}
  Node(const int& id) : _id(id), _lineNum(0),_entry(false), _sink(false), _violation(false), _funcEntrance(false), _funcReturn(false), _funcCall(false), _extern(false), _assume(false) {}
  ~Node() {}
  int getId() const ;
  void setLineNum(int);
  int getLineNum() const ;
  void setNext(int);
  int getNext() const ;
  bool operator< (const Node&) const;
  
  void setDescription(std::string) ;
  std::string getDescription();
  void setScope(std::string);
  std::string getScope();

  void setFuncEntrance();
  bool isFuncEntrance();
  void setFuncReturn();
  bool isFuncReturn();
  void setFuncCall();
  bool isFuncCall();

  void setExtern();
  bool isExtern();
  void setAssume(std::string);
  bool isAssume();

  std::string getFunc();

  void setEntry();
  void setSink();
  void setViolation();
  void printNode(); 
private: 
  int _id; // Node: ID / Edge: Source
  int _lineNum; // Edge: Startline
  int _next = _id+1; // Edge: Target
  std::string _description; // Edge: Source Code
  std::string _scope; // Edge: Assumption Scope
  std::string _assumption; // Edge: Assumption
  bool _entry; // Node: Entry
  bool _sink; // Node: Sink
  bool _violation; // Node: Violation
  bool _funcEntrance; // FOR HELP 
  bool _funcReturn; // Edge: Return from function
  bool _funcCall; // Edge: Enter function
  bool _extern;
  bool _assume;
};

#endif//__NODES_H__
