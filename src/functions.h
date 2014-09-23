#include <Rcpp.h>
using namespace Rcpp;

extern void Must_Be_Of_Length (const std::string COLL, const List coll, const List lengths);
extern void Must_Be_Collection_Of_Equal_Length (const std::string COLLS, const List colls);
extern void Must_Be_Flag (const std::string PRED, const SEXP flag);
extern SEXP try_higher_order_function(Language EXPR);
extern std::string delimit (const std::string str, const List strs);
extern std::string deparseInt (const int num);
extern List deparseInts (const List nums);
extern std::string dquote (const std::string str);
extern List concat (List coll0, List coll1);
extern std::vector<int> indices_to (int num);