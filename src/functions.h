#include <Rcpp.h>
using namespace Rcpp;

extern void Must_Be_Of_Length (const std::string COLL, const List coll, const List lengths);
extern std::string delimit (const std::string str, const List strs);
extern std::string deparseInt (const int num);
extern List deparseInts (const List nums);
extern std::string dquote (const std::string str);
