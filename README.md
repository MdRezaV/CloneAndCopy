# CloneAndCopy

A Roslyn source generator for C# that automatically implements `Clone()` and `CopyFrom()` methods for classes and structs, eliminating boilerplate code.

## Features

- **`Clone()` method** – Creates a shallow or deep copy of an object using object initializers.
- **`CopyFrom()` method** – Copies property values from one object to another.
- **Flexible property selection** – Choose between opt-in (explicit) or opt-out (implicit) via attributes.
- **Attribute-driven** – Control copying behavior on a per-property basis.
- **Partial class support** – Generated code is added as a partial class, so your existing code remains unchanged.

## Installation

Add the NuGet package to your project:

```bash
dotnet add package CloneAndCopy
```

## Usage

### 1. Mark your class as cloneable and/or copyable

Decorate your class with `[Cloneable]` and/or `[Copyable]`. The generator will then add a `Clone()` method or a `CopyFrom()` method to the class.

```csharp
using CloneAndCopy;

[Cloneable]
[Copyable]
public partial class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public Address Address { get; set; }
}
```

After generation, you can use:

```csharp
var original = new Person { Name = "John", Age = 30, Address = new Address { City = "NYC" } };
var copy = (Person)original.Clone();        // returns a new Person with copied properties
var another = new Person();
another.CopyFrom(original);                 // copies values from original into another
```

### 2. Control which properties are included

By default (implicit mode), **all writable properties** are included. You can exclude specific properties with `[IgnoreClone]` / `[IgnoreCopy]`.

```csharp
[Cloneable]
public partial class Person
{
    public string Name { get; set; }

    [IgnoreClone]      // not copied during Clone()
    public int Age { get; set; }

    public Address Address { get; set; }
}
```

To **opt-in** only selected properties, set `ExplicitDeclaration = true` on the class attribute and mark the desired properties with `[Clone]` / `[Copy]`:

```csharp
[Cloneable(ExplicitDeclaration = true)]
[Copyable(ExplicitDeclaration = true)]
public partial class Person
{
    [Clone] [Copy]
    public string Name { get; set; }

    public int Age { get; set; }      // not copied
}
```

### 3. Deep copying behavior

For properties that are themselves `[Cloneable]`, the generator automatically performs a deep copy by calling their `Clone()` method. You can prevent this deep copy for a specific property using `PreventDeepCopy = true`:

```csharp
[Cloneable]
public partial class Person
{
    public string Name { get; set; }

    [Clone(PreventDeepCopy = true)]
    public Address Address { get; set; }   // shallow copy only
}
```

### 4. Struct support

The attributes also work on `struct` types:

```csharp
[Cloneable]
public partial struct Point
{
    public int X { get; set; }
    public int Y { get; set; }
}
```

## Attributes Reference

| Attribute | Applies to | Description |
|-----------|------------|-------------|
| `[Cloneable]` | Class / Struct | Marks the type to receive a `Clone()` method. |
| `[Copyable]` | Class / Struct | Marks the type to receive a `CopyFrom()` method. |
| `[Clone]` | Property | Includes the property in the generated `Clone()` method (requires `ExplicitDeclaration = true`). |
| `[Copy]` | Property | Includes the property in the generated `CopyFrom()` method (requires `ExplicitDeclaration = true`). |
| `[IgnoreClone]` | Property | Excludes the property from `Clone()` (implicit mode). |
| `[IgnoreCopy]` | Property | Excludes the property from `CopyFrom()` (implicit mode). |

### Named arguments

- **`[Cloneable]` / `[Copyable]`**: `ExplicitDeclaration` (`bool`, default `false`).  
  If `true`, only properties explicitly marked with `[Clone]` or `[Copy]` are copied. If `false`, all writable properties are copied except those marked with `[IgnoreClone]` or `[IgnoreCopy]`.

- **`[Clone]` / `[Copy]`**: `PreventDeepCopy` (`bool`, default `false`).  
  If `true`, the property is assigned directly (shallow copy) even if the property type also has a `Clone()` method.

## Generated code

The generator adds a partial class definition to your source tree. For a class `Person` with `[Cloneable]`, you might see a generated file like `Person_cloneable.cs`:

```csharp
namespace YourNamespace
{
    public partial class Person
    {
        public object Clone()
        {
            return new Person
            {
                Name = this.Name,
                Age = this.Age,
                Address = (Address)this.Address.Clone()
            };
        }
    }
}
```

Similarly, `[Copyable]` generates:

```csharp
public void CopyFrom(object other)
{
    if (other is Person c)
    {
        Name = c.Name;
        Age = c.Age;
        Address = c.Address;
    }
}
```

## Limitations

- **Circular references** – The generated methods do not detect cycles. Calling `Clone()` on a graph with circular references will cause a `StackOverflowException`.
- **Inheritance** – The generated code does not automatically call base class cloning. You may need to implement manually if your class inherits from a base class that also uses this generator (or implement your own logic).

## Contributing

Contributions are welcome! Please open an issue or pull request on the [GitHub repository](https://github.com/MdRezaV/CloneAndCopy).

## License

This project is licensed under the terms of the [LICENSE](LICENSE) file.
