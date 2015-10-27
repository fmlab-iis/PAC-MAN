#include "nodes.h"

int Node::getId() const {
  return _id;
}
void Node::setLineNum(int i) {
  _lineNum = i;
}
int Node::getLineNum() const {
  return _lineNum;
}
void Node::setNext(int i) {
  _next = i;
}
int Node::getNext() const {
  return _next;
}
bool Node::operator< (const Node& n) const {
  if (_id < n._id) return true;
  else return false;
}

void Node::setDescription(std::string s) {
  _description = s;
}
std::string Node::getDescription() {
  return _description;
}
void Node::setScope(std::string s) {
  _scope = s;
}
std::string Node::getScope() {
  return _scope;
}
void Node::setFuncEntrance() {
  _funcEntrance = true;
}
bool Node::isFuncEntrance() {
  return _funcEntrance;
}
void Node::setFuncReturn() {
  _funcReturn= true;
}
bool Node::isFuncReturn() {
  return _funcReturn;
}
void Node::setFuncCall() {
  _funcCall = true;
}
bool Node::isFuncCall() {
  return _funcCall;
}
void Node::setExtern() {
  _extern = true;
}
bool Node::isExtern() {
  return _extern;
}
void Node::setAssume(std::string s) {
  _assume = true;
  _assumption = s;
}
bool Node::isAssume() {
  return _assume;
}

std::string Node::getFunc() {
  std::string ret;
  if (_funcCall) {
    if (_description.find(" = ") != std::string::npos) {
      ret = _description.substr(_description.find("=")+2);
      ret = ret.substr(0, ret.find("("));
    }
    else {
      ret = _description.substr(0, _description.find("("));
    }
  }
  else ret = "";
  return ret;
}


void Node::setEntry() {
  _entry = true;
}
void Node::setSink() {
  _sink = true;
}
void Node::setViolation() {
  _violation = true;
}
void Node::printNode() {
#ifdef DEBUG 
  std::cout << _id 
    << (_sink ? "" : " (Next: ")
    << (_sink ? "" : std::to_string(_next)) 
    << (_sink ? "" : ")") 
    << " (Scope: " << _scope << ")"
    << " (line " << _lineNum << ")"
    << " (" << _description << ")"
    << (_assume ? " (Assumption: " : "")
    << (_assume ? _assumption : "")
    << (_assume ? ")" : "") 
    << (_entry ? " (Entry)" : "") 
    << (_sink ? " (Sink)" : "") 
    << (_violation ? " (Violation)" : "") 
    << (_funcCall ? " (Call " : "") 
    << (_funcCall ? getFunc() : "") 
    << (_funcCall ? ")" : "") 
    << (_funcEntrance ? " (Entering)" : "") 
    << (_funcReturn ? " (Returning)" : "")
    << (_extern ? " (Extern)" : "") 
    << (_sink ? "\n" : " -> \n" );
#else
  // Print node info
  std::cout << "<node id=\"" << _id;
  if (!_entry && !_sink) std::cout << "\"/>\n";
  else {
    std::cout << "\">\n"; 
    if (_entry) 
      std::cout << "<data key=\"entry\">true</data>\n";
    /*
    if (_sink) 
      std::cout << "<data key=\"sink\">true</data>\n";
    if (_violation) 
      std::cout << "<data key=\"violation\">true</data>\n";
    */
    std::cout << "</node>\n";
  } 
  // TODO: Print edge info
  if (!_sink) {
    std::cout << "<edge source=\"" << _id << "\" target=\"" << _next << "\">\n";
    std::cout << "<data key=\"sourcecode\">" << _description << "</data>\n";
    std::cout << "<data key=\"startline\">" << _lineNum << "</data>\n";
    if (_assume) {
      std::cout << "<data key=\"assumption\">" << _assumption << "</data>\n";
      std::cout << "<data key=\"assumption.scope\">" << _scope << "</data>\n";
    }
    if (_funcCall && getFunc().find("VERIFIER") == std::string::npos) {
      std::cout << "<data key=\"enterFunction\">" << getFunc() << "</data>\n";
    }
    if (_funcReturn) {
      std::cout << "<data key=\"returnFrom\">" << _scope << "</data>\n";
    }
    std::cout << "</edge>\n";
  }
  else {
    std::cout << "<edge source=\"" << _id << "\" target=\"" << _id+1 << "\">\n";
    std::cout << "<data key=\"sourcecode\">" << _description << "</data>\n";
    std::cout << "<data key=\"startline\">" << _lineNum << "</data>\n";
    std::cout << "</edge>\n";
    std::cout << "<node id=\"" << _id+1 << "\">\n";
    std::cout << "<data key=\"sink\">true</data>\n";
    std::cout << "<data key=\"violation\">true</data>\n";
    std::cout << "</node>\n";
  }
#endif
}
