A small R package that contains a parser for data sheets. It is currently used to parse our google sheets with the BioSB curriculum.

The package exports only one function: `parse_sheet()`, which parses one sheet and returns a list of named tables. The structure of a data sheet should be as follows

```
# Comment rows start with a hash in the left most cell
# Blank lines are ignored, also if they contain tabs
# The name of a table is printed above a table behind the > symbol
# A table starts with a column header row in the left-most cell of a line

> name_of_table
# more optional comments or blank lines
var1  var2  var3
1 2 3
4 5 6

# more comments if needed
> other_table
var5  var6  var7
1 2 3
4 5 6
```

This sheet will be parsed by `parse_sheet()` and returns a list object with two named `tibble` objects 
