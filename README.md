# ds30
dark side of Seiran30

## Synopsis

in RMXP, RMVX and RMVXA

```ruby
$:<<"E:/fun/ds30" # replace with your directory
Kernel.require 'ds30.rb'
class Object
  include DSeiran30
end

class A
  include ExtRuby::ClassModule
end

x = A.new
p x.eval "3+5"
p x.eval %{require 'json'
JSON.load '{"a": 3, "b": {"c": 5, "d": [1,2,3,4,5]}}'
}
p x.eval %{
  require 'net/http'
  Net::HTTP.get URI 'https://rpg.blue'

}

p x.eval "RUBY_DESCRIPTION"
x.exit
```
