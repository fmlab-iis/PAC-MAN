/* XXX: ONLY WORKS ON C FILES PROCESSED BY CIL
 * Every line in the original file will result in a new node.
 * Extern functions are stored in the last node in the vector.
 * The std::map functions maps a string (the function name) 
 * to a pair of int, where the first int indicates the start 
 * of the function, and the second indicates the return.
 * A function return can be distinguished by two consecutive 
 * right braces.
 */ 
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include "nodes.h"
#include "utils.h"

using namespace std;


int main(int argc, char** argv)
{
  if (argc != 2) {
    cout << "Usage: ./genWitness [File]\n"; 
    return -1;
  }
  string filename = argv[1];
  ifstream ifs(filename.c_str(), ifstream::in);
  if (ifs) {
    string s;
    vector<Node> nodes; // Store all the nodes
    map<string, pair<int, int>> functions; // Store all the functions
    vector<string> ext; // Store external function names
    int sink_id = 0; // same as violation
    int entry_id = 0;
    ext.push_back("__VERIFIER_assume");
    ext.push_back("__VERIFIER_error");
    int line_number = 0; // Line number in original file
    bool start = false; // Set to TRUE when wthe first "line" is encountered
    bool possibleFuncReturn = false; // Set to TRUE when a single right brace is encountered
    string tmp_func; // The function to return from
    while (getline(ifs, s)) {
      s.erase(0, s.find_first_not_of(" \t\r\n"));
      size_t is_line = s.find("line");
      size_t is_extern = s.find("extern");
      // Create edge
      // Record line number in original file
      if (is_line != string::npos) {
        string line_number_str = s.substr(is_line+5);
        size_t a = line_number_str.find_first_of(" ");
        line_number = stoi (line_number_str.substr(0, a));
        if (!start) start = true;
      }
      else if (is_extern == string::npos) {
        if (start) {
          if (!checkTrivial(s)) { // If the string only includes left brace
            if (!checkFunctionReturn(s)) { // If the string is not a single right brace
              possibleFuncReturn = false;
              Node new_node(nodes.size());
              new_node.setDescription(s);
              new_node.setLineNum(line_number);
              if (s.find("main") != string::npos) { // If declaring the main function
                if (s.find(";") == string::npos) { // If not function prototype
                  entry_id = nodes.size(); 
                  new_node.setEntry();
                }
              }
              if (s.find("error") != string::npos) { // If error was reached
                sink_id = nodes.size(); 
                new_node.setViolation();
                new_node.setSink();
              }
              if (checkFunctionEntrance(s)) { // When entering function body
                size_t pos = s.find_first_of(" ")+1;
                string func_name = s.substr(pos, s.find('(')-pos);
                functions[func_name] = make_pair(new_node.getId(), 0);
                tmp_func = func_name;
                new_node.setFuncEntrance();
              }
              else { // Check if the string contains function call
                for (auto i = functions.begin(); i != functions.end(); i++) {
                  if (s.find(i->first) != string::npos)
                    new_node.setFuncCall();
                }
                // XXX: EXT IS REDUNDANT
                for (auto i = ext.begin(); i != ext.end(); i++) {
                  if (s.find(*i) != string::npos)
                    new_node.setFuncCall();
                }
                // XXX
              }
              nodes.push_back(new_node);
              if (s.find("error") != string::npos) break; // If error is reached, then end
            }
            else { // If the string IS a single right brace
              if (!possibleFuncReturn) possibleFuncReturn = true;
              else {
                nodes[nodes.size()-1].setFuncReturn();
                functions[tmp_func].second = nodes.size()-1;
                possibleFuncReturn = false;
              } 
            }
          }
        } 
      }
      else {
        // assume and error are not necessarily declared extern
        if (s.find("error") == string::npos && s.find("assume") == string::npos) {
          string t = s.substr(s.find_first_of("n")+2);
          size_t pos = t.find_first_of(" ")+1;
          string func_name = t.substr(pos, t.find('(')-pos);
          ext.push_back(func_name);
        }
      }
    }
    // Handle extern functions
    Node extern_node(nodes.size());
    extern_node.setDescription("Extern Functions");
    extern_node.setExtern();
    nodes.push_back(extern_node);
    for (auto i = ext.begin(); i != ext.end(); i++) {
      functions[*i] = make_pair(extern_node.getId(), extern_node.getId());
    }
    // Set assumptions
    // Also set function calls in case some were not set in the first go
    for (auto i = nodes.begin(); i != nodes.end(); i++) {
      if (i->getDescription().find("__VERIFIER_assume") != string::npos) {
        string as = i->getDescription().erase(0, i->getDescription().find("assume")+6);
        as = as.substr(0, as.find_last_of(")")+1);
        i->setAssume(as);
      }
      for (auto f = functions.begin(); f != functions.end(); f++){
        if (i->getDescription().find(f->first) != string::npos) 
          if (!i->isFuncEntrance())
            i->setFuncCall();
      }
    }
    // Set the scope of each called functions
    traverse(nodes, functions, entry_id, nodes.size()-1, "main", -1);

#ifdef DEBUG
    for (auto i = nodes.begin(); i != nodes.end(); i++) {
      i->printNode();
      cout << endl;
    }
    for (auto i = functions.begin(); i != functions.end() ; i++) {
      cout << i->first << " @ " << i->second.first << "->" << i->second.second << endl;
    }
    cout << "\n\n\n============= HUMAN READABLE ============\n";
    printGraph(nodes, functions, entry_id, nodes.size()-1);
#else
    printMisc(filename);
    printGraph(nodes, functions, entry_id, nodes.size()-1);
#endif
  }
  return 0;
}
