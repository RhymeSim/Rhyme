# Nombre
A simple package to handle easy arithmetic, unit conversion and error calculation


# Usage
We can use `.unit` or `.u.` operator to define new numbers as follow,

```Fortran
program HubbleConstant
  use Nombre

  type(nombre_t) :: H

  H = 66.7 .unit. (km / s) / Mpc
end program HubbleConstant
```

Now we can simply convert its unit to a desired one using `.to` operator (note
right now we don't check the dimension equality of the number, before and after
conversion),

```Fortran
H_s = H .to. s**(-1)
```
