=begin
Dark Side Seiran30
=end

=begin
  ? of 30  
  1. Some None
  2. SExp
  3. Subst Unify
  4. API
     Memory
     Value
     CFunc
     AsyncCall
  5. Defer Disposer
=end
module DSeiran30
    module General
        module Module
            def deferror(name)
                const_set name, Class.new(Exception)
            end
            def declerror(*name)
                name.each{|x| deferror(x) }
            end
        end
    end
    Some = Struct.new :val
    None = Class.new
    NoneVal = None.new

    class Some
        def bind(&rhs)
            rhs.call(val)
        end
    end

    class None
        def bind(&rhs)
            NoneVal
        end
    end

    class SExp
        def initialize(*args)
            @sub = args
        end
        def [](a)
            @sub[a]
        end
        def []=(a, b)
            @sub[a] = b
        end
        def size
            @sub.size
        end
        def each(*a, &b)
           return to_enum(:each, *a) if defined?(to_enum) && !b
           @sub.each(*a, &b) 
           self
        end
        def map(*a, &b)
            return to_enum(:map, *a) if defined?(to_enum) && !b
            @sub.map(*a, &b) 
        end
        def inspect
            "s(" << map{|x| x.inspect }.join(", ") << ")"
        end
        def occur?(sym)
            each{|x|
                case x
                when SExp
                    return true if x.occur?(sym)
                when Symbol
                    return true if sym == x
                end
            }
            false
        end

        def deepclone
            SExp.new(*map{|x|
                case x
                when SExp then x.deepclone
                when Symbol, Integer then x
                else x.clone
                end
            })
        end
    end

    class SExp  # match
        attr_accessor :subst
        def ===(rhs)
            s = Subst.new
            r = s.unify(self, rhs)
            if Some === r
                rhs.subst = r.val
                true
            else
                false
            end
        end
    end

    def s(*args)
        SExp.new(*args)
    end

    def st(*args)
        s(*args.map{|x|
            case x
            when SExp, Symbol then x
            when Array
                st(*x)
            when Integer
                s(:@int, x)
            when String
                s(:@str, x)
            when Float
                s(:@float, x)
            else
                s(:typed, x.class, x)
            end
        })
    end

    module ThreadScope
        module Impl
            def thread_stack(name)
                Thread.current[:stack] ||= {}
                Thread.current[:stack][name] ||= []
            end
        end

        module Module
            include Impl
        end
    end

    module Disposer
        module Impl
            
        end
        module Module
            include ThreadScope::Module
            def disposer_fns
                @__fns ||= []
            end

            def at_dispose(fn = nil, &bl)
                disposer_fns << (fn || bl)
            end

            def call_disposer(obj)
                disposer_fns.each{|x|
                    x.call(obj)
                }
            end

            def define_dispose
                n = if method_defined?(:dispose)
                    instance_method(:dispose)
                end
                define_method(:dispose) do
                    self.class.call_disposer(self)
                    if n
                        n.bind(self).call
                    end
                end
                define_method(:mark) do
                    self.class.thread_stack(Disposer)[-1] << self if self.class.thread_stack(Disposer)[-1] 
                    self
                end
            end
        end

        class Class
            include ThreadScope::Module
            def initialize
                @objs = []
                if block_given?
                    thread_stack(Disposer) << self
                    yield self
                    dispose
                    thread_stack(Disposer).pop
                end
            end

            def new(klass, *args, &block)
                r = klass.new(*args, &block)
                @objs << r
                r
            end

            def <<(obj)
                @objs << obj
            end
            
            def dispose
                @objs.reverse.each{|x|
                    x.dispose if defined?(x.dispose)
                }
            end
        end
        Scope = Class
    end
  
    module Fresh
        def fresh
            @__fresh       ||= 0
            @__freshprefix ||= "_id"
            :"#{@__freshprefix}#{@__fresh += 1}"
        end
    end

    class Subst
        include Fresh
        attr_accessor :subst
        def initialize(subst = {})
            @subst = subst
        end

        def set(a, b)
            @subst[a] = b
        end

        def get(a)
            if @subst.include?(a)
                @subst[a]
            else
                a
            end
        end

        def [](a)
            get a
        end

        def []=(a, b)
            x = apply(b)
            y = get(x)
            if y != x
                set a, y
            else
                set a, x
            end
        end

        def apply(sexp)
            case sexp
            when SExp
                s(sexp[0], *sexp[1..-1].map{|x| apply(x)})
            else
                y = get(sexp)
                if y != sexp
                    apply y
                else
                    y
                end
            end
        end

        def inspect
            "subst(" << self.subst.map{|k, v| "#{k.inspect} -> #{v.inspect}"}.join(", ") << ")"
        end

        def +(rhs)
            h = {}.update(self.subst).update(rhs.subst)
            Subst.new(h)
        end

        def add(a, b)
            h = {}.update(self.subst).update(a => b)
            Subst.new(h)
        end

        def add!(a, b)
            set a, b
            self
        end

        def substall(a, b)
            sone = Subst.new(a => b)
            h = {}.update(self.subst)
            g = {}
            h.each{|k, v|
                g[k] = sone.apply(v)   
            }
            Subst.new(g)
        end

        def unification(a, b)
            subst = unify(a, b)
            if Some === subst
                subst = subst.val
                aa = subst.apply(a)
                Some.new aa
            else
                None
            end
        end

        def unify(a, b)
            subst = Subst.new({}.update(self.subst))
            sym1, sym2 = Symbol === a, Symbol === b
            case [sym1, sym2]
            when [true, true]
                if a == b
                    Some.new subst
                else
                    r = subst.apply(b)
                    if r != b
                        Some.new(subst.substall(a, r).add(a, r))
                    else
                        r = subst.apply(a)
                        if r != a
                            Some.new(subst.substall(b, r).add(b, r))
                        else
                            name = fresh
                            Some.new(subst.substall(a, name).substall(b, name).add(a, name).add(b, name))
                        end
                    end
                end
            when [false, true]
                unify b, a
            when [true, false]
                if SExp === b && b.occur?(a)
                    NoneVal
                else
                    r = subst.apply(a)
                    if r != a                        
                        x = subst.unify(r, b)
                        if Some === x
                            Some.new(x.substall(a, b).add(a, b))
                        else
                            NoneVal
                        end
                    else
                        Some.new(subst.substall(a, b).add(a, b))
                    end
                end
            when [false, false]
                if SExp === a && SExp === b
                    if a[0] != b[0] || a.size != b.size
                        NoneVal
                    else
                        x = subst
                        (1...a.size).each{|i|
                            r = x.unify(a[i], b[i])
                            if None === r then
                                return None
                            else
                                x = r.val
                            end
                        }
                        Some.new x
                    end
                else
                    if a == b
                        Some.new subst
                    else
                        NoneVal
                    end
                end
            end            
        end
    end

    
end

=begin
    API
=end

module DSeiran30
    module BasicAPI
        module Impl
            extend self
            def win32api
                begin
                    Win32API
                rescue LoadError
                    require 'win32api'
                    retry
                end
            end
        
            def api(a, b)
                lambda{|*args| apicall(a, b, *args)}
            end
        
            def apicall(a, b, *args)
                params = args.map{|x|
                    case st(:foo, x)
                    when s(:foo, s(:@int, :a))
                        "L"
                    else
                        "p"
                    end
                }
                win32api.new(a, b, params, "L").call(*args)
            end

            def _to_ws_internal(str)
                str.unpack("U*").pack("S*") + "\0\0"
            end

            def loadlibrary(name)
                apicall("Kernel32", "LoadLibraryW", _to_ws_internal(name))
            end

            def freelibrary(handle)
                apicall("Kernel32", "FreeLibrary", handle)
            end

            def funcaddr(a, b)
                handle = apicall("Kernel32", "GetModuleHandle", a)
                if handle == 0
                    raise "Module #{a} must be loaded first"
                end
                apicall("Kernel32", "GetProcAddress", handle, b + "\0")
            end
        end

        module Module
            def native_library(name)
                Impl.loadlibrary name
            end
            
            def defapi(a, b, c = b)
                define_method(c) do |*args|
                    Impl.apicall(a, b, *args)
                end
            end

            def _internal_defapi(c, a, *b)
                b.each{|name| 
                    case name
                    when String, Symbol
                        send c, a, name
                    when Hash
                        name.each{|k, v|
                            send c, a, k, v
                        }
                    end
                }
            end

            def declapi(a, *b)
                _internal_defapi :defapi, a, *b
            end

            def funcaddr(a, b)
                CFunc.new(Memory.new(Impl.funcaddr(a, b)))
            end
        end
    end

    class Memory
        extend BasicAPI::Module
        declapi 'Kernel32', 'RtlMoveMemory' => :copy
        declapi 'msvcrt',  'free' => :cfree

        class << self
            extend BasicAPI::Module
            declapi 'msvcrt', 'malloc' => :cmalloc, 'memset' => :cmemset
        end
        
        def initialize(ptr, len = nil)
            @ptr = ptr
            @len = len
        end

        def free
            cfree @ptr if @ptr
            @ptr = nil
        end

        def address
            @ptr
        end

        def length
            @len
        end

        alias to_int address

        def +(rhs)
            self.class.new @ptr + rhs
        end

        def [](a, b)
            (0...b).map{|i|
                ch = "\0"
                copy ch, @ptr + a + i, 1
                ch.unpack("C").first
            }
        end

        def []=(a, b, c)
            len = [b, c.size].min
            len.times{|i|
                copy @ptr + a + i, [c[i]].pack("C"), 1
            }
        end

        def self.malloc(len, fill = nil)
            ptr = cmalloc len
            if ptr != 0
                if fill
                    cmemset ptr, fill, len
                end
                Memory.new ptr, len
            else
                raise "Can't allocate #{len} byte(s)"
            end
        end
    end

    module SysWaitable
        extend General::Module
        extend BasicAPI::Module
        declapi 'Kernel32', 'WaitForSingleObject' => :wait
        declerror :Failed, :Abandoned
        
        def handle
            raise "Must implement handle"
        end

        def awaitforever(&block)
            while 1
                r = wait handle, 10
                case r
                when 0xFFFFFFFF
                    raise SysWaitable::Failed
                when 0x102
                    yield
                when 0x80
                    raise SysWaitable::ABANDONED
                when 0
                    break
                end
            end
        end
        
        def alive?
            !await(0.01)
        end

        def await(timeout = nil)
            timeout = timeout ? (timeout * 1000).to_i : -1 
            r = wait handle, timeout
            case r
            when 0xFFFFFFFF
                raise SysWaitable::Failed
            when 0x102
                false
            when 0x80
                raise SysWaitable::ABANDONED
            when 0
                true
            end
        end 
    end

    class Value
        def initialize(ptr, pack, len)
            @ptr, @pack, @len = ptr, pack, len
        end

        def address
            @ptr.address
        end

        def [](*a)
            @ptr[0, @len].pack("C*").unpack(@pack).first
        end

        def []=(val)
            @ptr[0, @len] = [val].pack(@pack).unpack("C*")
        end

        def getclose
            val = self[]
            @ptr.free
            val
        end

        def self.int
            Value.new(Memory.malloc(4), "L", 4)
        end
    end

    class NativeThread
        extend BasicAPI::Module
        declapi 'Kernel32', 'GetExitCodeThread' => :valueof

        class << self
            extend BasicAPI::Module
            declapi 'Kernel32', 'CreateThread' => :native_thread
        end

        include SysWaitable
        attr_reader   :handle
        attr_accessor :onterminate

        def initialize(handle)
            @handle = handle
            @value  = nil
        end
        
        def value
            @value ||= begin
                awaitforever { yield }
                int = Value.int
                valueof @handle, int.address
                ret = int.getclose
                dispose
                ret
            end
        end

        def dispose
            if onterminate
                onterminate.call
                onterminate = nil
            end
        end

        def self.fromMem(mem, arg = 0, &block)
            thread = new(native_thread(0, 0, mem.address, arg, 0, 0))
            thread.onterminate = block
            thread
        end
    end

    class CFunc
        extend BasicAPI::Module
        declapi 'user32', 'CallWindowProcW' => :nativecall
        def initialize(ptr, type = :stdcall)
            @ptr = ptr
            @type = type
        end

        def callstub(*args)
            out = []
            out << 0x55
            out << 0x8b << 0354
            args.reverse.each{|x|
                case x
                when Integer
                    out << 0x68
                    out.concat [x].pack("L").unpack("C*")
                when String
                    out << 0x68
                    out.concat [x].pack("p").unpack("C*")
                end
            }
            out << 0xb8
            out.concat [@ptr.address].pack("L").unpack("C*")
            out << 0xff << 0320
            if type == :cdecl
                out.concat [0x83, 0304, 4 * args.length]
            end
            out << 0xc9
            out << 0xc2 << 16 << 0
            out
        end

        def call(*args)
            code = callstub(*args)
            mem  = Memory.malloc code.length
            mem[0, code.length] = code
            ret = nativecall mem.address, 0, 0, 0, 0
            mem.free
            ret
        end

        def async(*args)
            code = callstub(*args)
            mem  = Memory.malloc code.length
            mem[0, code.length] = code
            NativeThread.fromMem(mem){
                mem.free
            }
        end
    end

    module AsyncAPI
        module Module
            include BasicAPI::Module
            def defasync(a, b, c = b)
                faddr = funcaddr(a, b)
                define_method(c) do |*args|
                    faddr.async(*args)
                end
            end

            def declasync(a, *b)
                _internal_defapi :defasync, a, *b
            end
        end
    end

    module Messaging
        class Impl
            extend AsyncAPI::Module
            declapi 'ws2_32', 'socket', 'connect', 'bind', 'sendto', 'recvfrom',
                             'closesocket'
            
                             
            class << self
                extend AsyncAPI::Module
                declapi 'ws2_32', 'WSAStartup'
                def init
                    @ws2 ||= begin
                        version = 0x202
                        @wss ||= "\0" * 1024
                        WSAStartup version, @wss
                    end
                end
                def local(port)
                    [[127, 0, 0, 1], port]
                end
            end
           

            def address
                "udp://#{LOCALHOST}:#{@port}"
            end

            attr_reader :port

            LOCALHOST = [127, 0, 0, 1]

            def _make_addr(host, port)
                r = host.pack("C*")
                p = [2, port].pack("sn")
                p + r + "\0" * 8
            end

            def _extract_addr(buf)
                host = buf[4, 4].unpack("C*")
                port = buf[2, 2].unpack("n").first
                [host, port]
            end

            def initialize(port = 10100..10200)
                self.class.init
                @socket = socket(2, 2, 0)
                port.each{|x|
                    if trybind(@socket, x)
                        @port = x
                        break
                    end
                }
            end

            def trybind(socket, port)
                addr = _make_addr LOCALHOST, port
                bind(@socket, addr, 16) != -1
            end

            def _sendto(saddr, data)
                host, port = saddr
                addr = _make_addr(host, port)
                sendto @socket, data, data.length, 0, addr, addr.length
            end

            def _recvfrom(len = 1024)
                buf = "\0" * len
                addr = "\0" * 64
                r = [64].pack("L")
                retlen = recvfrom @socket, buf, len, 0, addr, r
                if retlen > 0 then
                    host, port = _extract_addr addr
                    Some.new [buf[0, retlen], [host, port]]
                else
                    NoneVal
                end
            end

            def sendtext(saddr, data)
                _sendto(saddr, data)
            end

            def recvtext(wait = nil, len = 1024)
                while 0
                    ret = _recvfrom(len)
                    if None === ret
                        wait.call if wait
                        next
                    end
                    buf, addr = ret.val 
                    sender    = lambda{|data|
                        sendtext(addr, data)
                    }
                    if block_given?
                        return yield sender, buf
                    else
                        return buf
                    end
                end
            end
        end

        Class = Impl
    end

    module ExtRuby
        module ImplMessaging
            def ruby
                @port = 19200
                @ruby ||= begin
                    @rubytext = <<EOF
require 'socket'
def myeval(a)
    eval a, TOPLEVEL_BINDING, "<port>", 1
rescue Exception
    $!.backtrace.unshift($!.to_s)
end
x = UDPSocket.new
x.bind '127.0.0.1', #{@port}
puts "Bind at 127.0.0.1 #{@port}"
while 0
    text, addr = x.recvfrom(10485760)
    puts text
    ret = Marshal.dump(myeval(text))
    x.send ret, 0, addr[2], addr[1]
    sleep 0.01
end
EOF
                    open("tmp.rb", "w") do |f|
                        f.write @rubytext
                    end
                    system "cmd /c start /min \"\" cmd /k ruby tmp.rb"
                    sleep 1
                    @addr = Messaging::Class.local @port 
                    Messaging::Class.new        
                end
            end

            def eval(str)
                r = ruby
                r.sendtext @addr, str
                Marshal.load r.recvtext(Graphics.method(:update), 10485760)
            end

            def exit
                r = ruby
                r.sendtext @addr, "exit!"
            end
        end

        ClassModule = ImplMessaging
    end

    

end
