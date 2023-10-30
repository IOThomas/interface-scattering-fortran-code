# Manyleb I/O

## Dosbarthiadau

### Input Variable

- io_variable (abstract class)
    - protected variables: name (string); units (string)
    - public function: output writing function, assign name, assign units

- real_variable extends io_variable
    - protected variables: value (real)
    - public function: assign value

- integer_variable extends io_variable
    - protected variables: value (integer)
    - public function: assign value

- complex_variable extends io_variable
    - protected variables: value (complex)
    - public function: assign value (complex)

- string_variable extends io_variable
    - protected variables: value (string)
    - public function: assign value (string)