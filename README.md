# Toy programming language #
Simple dynamically typed language. 

In this small language all functions are first class objects.
Structure and function declarations are missing, that's why one must use this way:
x = <function or structure object definition>
There are four types: Int, Double, String and Boolean.
Also we have special value: Undefined.

## Syntax overview ##

Variable declaration:
```
x = 10;
```

Struct definition:
```
x = [
    y = 1;
    z = 2;
];
```

Function definition:
```
f = (y = 1) => {
    y + 1;
};
```

For generator:
```
for (i <- 1 to 10) {
    println(i);
}
```

While cycle:
```
while (true) {
    println(1);
}
```

If expression:
```
x = if (1 < 2) {
    10;
} else {
    12;
};
```

Call expression:
```
f = () => {};
f();
```

Invoke expression:
```
s = [x = 1;];
println(s.x);
```

## Building && running toy language ##
Import project sources to IntelliJ IDEA or Eclipse.



