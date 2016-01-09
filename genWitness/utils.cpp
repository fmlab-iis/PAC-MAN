#include "utils.h"

using namespace std;

bool checkTrivial(const string& s) {
  if (s.find_first_not_of("{") == string::npos)
    return true;
  else if (s[s.find_first_not_of("{")] == ' ')
    return true;
  else 
    return false;
}

bool checkFunctionEntrance(const string& s) {
  if (s.find_first_of("=") == string::npos
      && s.find("main") == string::npos) {
    string t = s.substr(s.find_first_of(")")+1);
    if (t.find_first_not_of(" \t\r\n") == string::npos)
      return true;
  }
  return false;
}

bool checkFunctionReturn(const string& s) {
  if (s.find_first_not_of("}") == string::npos)
    return true;
  else return false;
}

void printMisc(string filename) { // TODO
  cout << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n<graphml xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://graphml.graphdrawing.org/xmlns\">\n";
  cout << "<key attr.name=\"assumption\" attr.type=\"string\" for=\"edge\" id=\"assumption\"/>\n";
  cout << "<key attr.name=\"sourcecode\" attr.type=\"string\" for=\"edge\" id=\"sourcecode\"/>\n"; 
  cout << "<key attr.name=\"sourcecodeLanguage\" attr.type=\"string\" for=\"graph\" id=\"sourcecodelang\"/>\n";
  cout << "<key attr.name=\"startline\" attr.type=\"int\" for=\"edge\" id=\"startline\"/>\n";
  cout << "<key attr.name=\"originFileName\" attr.type=\"string\" for=\"edge\" id=\"originfile\">\n";
  cout << "<default>" << filename << "</default>\n"; // TODO
  cout << "</key>\n";
  cout << "<key attr.name=\"nodeType\" attr.type=\"string\" for=\"node\" id=\"nodetype\">\n";
  cout << "<default>path</default>\n";
  cout << "</key>\n";
  cout << "<key attr.name=\"isViolationNode\" attr.type=\"boolean\" for=\"node\" id=\"violation\">\n";
  cout << "<default>false</default>\n";
  cout << "</key>\n";
  cout << "<key attr.name=\"isEntryNode\" attr.type=\"boolean\" for=\"node\" id=\"entry\">\n";
  cout << "<default>false</default>\n";
  cout << "</key>\n";
  cout << "<key attr.name=\"isSinkNode\" attr.type=\"boolean\" for=\"node\" id=\"sink\">\n";
  cout << "<default>false</default>\n";
  cout << "</key>\n";
  cout << "<key attr.name=\"enterFunction\" attr.type=\"string\" for=\"edge\" id=\"enterFunction\"/>\n";
  cout << "<key attr.name=\"returnFromFunction\" attr.type=\"string\" for=\"edge\" id=\"returnFrom\"/>\n";
  cout << "<graph edgedefault=\"directed\"><data key=\"sourcecodelang\">C</data>\n";
  cout << "<data key=\"producer\">PACMAN</data>\n";
  cout << "<data key=\"programfile\">" << filename << "</data>\n"; // TODO
  cout << "<data key=\"memorymodel\">precise</data>\n";
  cout << "<data key=\"architecture\">32bit</data>\n";
}

void printGraph(vector<Node>& nodes, map<string, pair<int, int>>& f, int b, int e) {
  for (int i = b; i <= e; i++) {
    if (!nodes[i].isExtern()) {
      nodes[i].printNode();
      if (nodes[i].isFuncCall()) {
        string n = nodes[i].getFunc();
        int bn = f[n].first, en = f[n].second;
        printGraph(nodes, f, bn, en);
      }
    }
  }
}

void printFuncSeq(vector<tuple<int, bool, string>>& funcSeq) {
  vector<string> funcName;
  cout << "<node id=\"ENTRY\">\n"
       << "<data key=\"entry\">true</data>\n"
       << "</node>\n";
  cout << "<edge source=\"ENTRY\" target=\"" << get<0>(funcSeq[0]) << "\"/>\n";
  for (size_t i = 0; i < funcSeq.size()-1; i++) {
    cout << "<node id=\"" << get<0>(funcSeq[i]) << "\"/>\n";
    cout << "<edge source=\"" << get<0>(funcSeq[i]) << "\" target=\"" << get<0>(funcSeq[i+1]) << "\">\n";
    if (get<1>(funcSeq[i]) == 0) {
      cout << "<data key=\"enterFunction\">" << get<2>(funcSeq[i]) << "</data>\n"
           << "</edge>\n";
      funcName.push_back(get<2>(funcSeq[i]));
    }
    else {
      cout << "<data key=\"returnFrom\">" << funcName.back() << "</data>\n"
           << "</edge>\n";
      funcName.pop_back();
    }
  }
  cout << "<node id=\"" << get<0>(funcSeq.back()) << "\"/>\n";
  cout << "<edge source=\"" << get<0>(funcSeq.back()) << "\" target=\"ERROR\">\n";
  cout << "<data key=\"returnFrom\">" << funcName.back() << "</data>\n"
       << "</edge>\n";
  funcName.pop_back();
  cout << "<node id=\"ERROR\">\n"
       << "<data key=\"violation\">true</data>\n"
       << "</node>\n";
}

void traverse(vector<Node>& nodes, vector<tuple<int, bool, string>>& funcSeq, map<string, pair<int, int>>& f, int b, int e, string s, int p) {
  for (int i = b; i <= e; i++) {
    if (!nodes[i].isExtern()) {
      nodes[i].setScope(s);
      if (nodes[i].isFuncCall()) {
        string n = nodes[i].getFunc();
        int bn = f[n].first, en = f[n].second;
        if (n.find("VERIFIER") == string::npos) {
          funcSeq.push_back(make_tuple(nodes[i].getId(), 0, n));
          nodes[i].setNext(bn);
          traverse(nodes, funcSeq, f, bn, en, n, i);
        }
      }
      else if (nodes[i].isFuncReturn()) {
        nodes[i].setNext(p+1);
        string n = nodes[i].getFunc();
        funcSeq.push_back(make_tuple(nodes[i].getId(),1, n));
      }
    }
  }
}

void changeOperator(string& s) {
  size_t gtp = s.find(" > ");
  if (gtp != string::npos)
    s.replace(gtp+1, 1, "&gt;");
  size_t ltp = s.find(" < ");
  if (ltp != string::npos)
    s.replace(ltp+1, 1, "&lt;");
  size_t gtoep = s.find(" >= ");
  if (gtoep != string::npos)
    s.replace(gtoep+1, 2, "&gt;=");
  size_t ltoep = s.find(" <= ");
  if (ltoep != string::npos)
    s.replace(ltoep+1, 2, "&lt;=");
}
