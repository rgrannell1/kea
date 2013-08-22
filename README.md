
Arrow v0.1
-----------------------------------

### Generic & Idiomatic
  
Arrow embrace R's better quirks - such as three-value logic and vectorisation - while . 

### Cascading Style

```javascript
# get every unique function parameter in the 
# R base library.

x_(ls("package:base"))$  
xMap(function (x) get(x))$
xSelect(is.function)$
xMap( function (f) names(xFormals(f)) )$
xReducel(union)$
x()

```

## Licensing

Arrow is released under the terms of the GNU General Public License version 3. 

<img src="gpl3.png" height = "180"> </img>
