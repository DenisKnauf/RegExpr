
class RegExpr< Hash
end

class RegExpr::Segment
	attr_accessor :value
	def initialize( val) self.value= val end
	def to_r() self.value.to_s end
	def empty?() self.value.nil? end
	def names() @value.names.flatten.compact end

	def optimize
		self.value= self.class.optimize self.value
		self
	end

	class <<self
		def optimize v
			v= v.optimize
			v= nil  if v and v.empty?
			v= v.value[ 0]  if RegExpr::Block === v and v.hidden and v.size == 1
			v
		end

		def deepest
			self.class_eval do
				def names() [] end
			end
		end

		def novalue
			self.class_eval do
				def initialize() end
				def empty?() false end
				def to_r() '' end
				def optimize() self end
			end
		end

		def nooptimize
			self.class_eval do
				def optimize() self end
			end
		end
	end
end

class RegExpr::Block< RegExpr::Segment
	attr_accessor :name, :hidden
	def hidden?() @hidden end
	def optimize() self.dup.optimize! end
	def push( *v) @value.push *v end
	def pop() @value.pop end
	def empty?() @value.empty? end
	def size() @value.size end

	def names
		names= @value.collect &:names
		names.push( name)  unless self.hidden?
		names.flatten.compact
	end

	def initialize *val
		val= val[ 0]  if ::Array === val[ 0] and val.size == 1
		super val
		@hidden= true
	end

	def optimize!
		list, chars= [[]], RegExpr::Chars.new( '')

		@value.each do |v|
			v= self.class.optimize v
			if RegExpr::Or === v
				list.push []
			else list[ -1].push v
			end
		end

		list.delete_if do |v|
			if (RegExpr::Chars === v[ 0] and v.size == 1 ) or RegExpr::Char === v[ 0]
				chars+= v[ 0]
			else false
			end
		end
		chars= chars.optimize

		values= []
		list.each do |v|
			values.push RegExpr::Or.new
			values+= if v.size == 1 and RegExpr::Block === v[ 0] and v[ 0].hidden
					v[ 0].value
				else
					v.collect do |w|
						if RegExpr::Block === w and w.hidden
							u= false
							w.value.each do |i|
								break  unless u||= RegExpr::Or === i
							end
							u ? w : w.value
						else w
						end
					end.flatten
				end
		end
		values.push RegExpr::Or.new, chars  if chars.size > 0
		values.shift
		@value= values
		self
	end

	def to_r()
		(@hidden ? '(?:%s)' : '(%s)')% @value.collect( &:to_r).join( '')
	end
end

class RegExpr::Not< RegExpr::Segment
	deepest
	novalue
	def to_r
		if @value.instance_of? RegExpr::Chars
			@value.not!
			@value.to_s
		else '(?!%s)'% @value
		end
	end
end

class RegExpr::Range< RegExpr::Segment
	novalue
	attr_accessor :v1, :v2
	def names() [] end
	def optimize() self.value.optimize end
	def to_r() self.optimize.to_r end
	def initialize( v1, v2) @v1, @v2= v1, v2 end

	# algo stolen from thomas leitner
	def value
		a, b= @v1< @v2 ? [ @v1, @v2] : [ @v2, @v1]
		arr= Array[ a]

		af= a == 0 ? 1.0 : a.to_f
		bf= b == 0 ? 1.0 : b.to_f
		1.upto( b.to_s.length- 1) do |i|
			pot= 10** i
			num= (af/ pot).ceil* pot   # next higher number with i zeros
			arr.insert i, num  if num < @v2
			num= (bf/ pot).floor* pot  # next lower number with i zeros
			arr.insert -i, num
		end
		arr.uniq!
		arr.push b+ 1  # +1 -> to handle it in the same way as the other elements

		result= RegExpr::Block.new
		0.upto( arr. length- 2) do |i|
			first= arr[ i].to_s
			second= (arr[ i+ 1]- 1).to_s
			result.push RegExpr::Or.new
			0.upto( first.length- 1) do |j|
				result.push( if first[ j] == second[ j]
						RegExpr::Char.new first[ j].chr
					else
						RegExpr::Chars.new '%c-%c'% [ first[ j], second[ j] ]
					end)
			end
		end
		result. value. shift
		result
	end
end

class RegExpr::Chars< RegExpr::Segment
	deepest
	attr_reader :chars, :not
	def to_r() '[%s]'% self. value end
	def not?() @not end
	def empty?() @chars. empty? end
	def size() @chars. size end
	def value=( val) @chars= (@not= val[ 0] == ?^) ? val[ 1.. -1] : val ; val end
	def value() (self. not? ? '^' : '')+ (@chars) end
	def not!() @not= !@not end
	alias -@ not!

	def split
		chars= []
		@chars. gsub( /\\-/) do |r|
			chars. push ?-.ord
			nil
		end. gsub( /.-./) do |r|
			chars+= (r[ 0].ord .. r[ 2].ord). to_a
			nil
		end. bytes. each do |c|
			chars. push c
		end
		chars
	end

	def optimize!
		b2chr= lambda do |b|
			b = b.chr
			"-[]".include?( b) ? '\%c'% b : b
		end
		chars= self. split. sort. uniq
		@chars= ''
		return self  if chars.empty?
		b= chars.shift
		chars.each do |i|
			if b+1 == i
				unless @chars[ -1] == ?- and @chars[-2] != ?\\
					@chars+= b2chr.call( b)+ '-'
				end
			else @chars+= b2chr.call b
			end
			b= i
		end
		@chars+= b2chr.call b
		self
	end

	def optimize
		n= self.dup.optimize!
		if (n.size == 1 or (n.size == 2 and n.value[ 0] == ?\\ )) and not n.not?
			RegExpr::Char.new n.chars[ -1]
		else
			n
		end
	end

	def + b
		chars= self.not? ? '^' : ''
		chars+= if b.instance_of? RegExpr::Char
			self.split.push b.value[ 0]
			elsif self.not? == b.not?
				self.split+ b.split
			elsif self.not?
				(0..255).to_a- self.split+ b.split
			else
				(0..255).to_a- b.split+ self.split
			end.compact.uniq.collect {|i| i.chr }.join( '')
		self.class.new chars
	end
end

class RegExpr::Repeat< RegExpr::Segment
	attr_reader :min, :max

	def minandmax x
		case x
		when nil, ''  then nil
		else x.to_i
		end
	end

	def optimize
		super
		r = (min == 1 and max == 1) ? @value : self
		r
	end

	def initialize value, min= 1, max= min
		super value
		@min, @max= self.minandmax( min), self.minandmax( max)
	end

	def to_r
		t= '{%s,%s}'% [ @min||'', @max||'' ]
		return ''  if '{0,0}' == t
		t= Hash[ *%w<{,1} ? {0,1} ? {0,} * {,} * {1,} +>+ ['{1,1}', ''] ][ t]|| t
		@value.to_r+ t
	end
end

class RegExpr::Char< RegExpr::Segment
	deepest
	nooptimize
	def to_r() ::Regexp.quote @value end
	def size() 1 end

	def self.new x
		x= x.split( '').collect {|i| super i }
		x.size == 1 ? x[ 0] : RegExpr::Block.new( x)
	end
end

class RegExpr::Regexp< RegExpr::Segment
	deepest
	nooptimize
	def to_r() @value. to_s end
end

class RegExpr::Or< RegExpr::Segment
	deepest
	novalue
	def to_r() '|' end
	def to_s() '|' end
end

class RegExpr::End< RegExpr::Segment
	deepest
	novalue
	def to_r() '$' end
	def to_s() '$' end
end

class RegExpr::Begin< RegExpr::Segment
	deepest
	novalue
	def to_r() '^' end
	def to_s() '^' end
end

class RegExpr::WildCard< RegExpr::Segment
	deepest
	nooptimize
	def to_r() @value end
	def to_s() @value end
end

class RegExpr
	class <<self
		STDEXP= Hash[
				'loalpha' => '[a-z]',
				'hialpha' => '[A-Z]',
				'alpha' => 'loalpha | hialpha',
				'digit' => '[0-9]',
				'alphadigit' => 'alpha | digit',
				'hexdigit' => 'digit | [a-fA-F]',
				'octdigit' => '[0-7]',
				'bindigit' => '[01]',
				'space' => '[ \t\n\r\v]'
			]

		def [] *vals
			ret= super *vals
			STDEXP.each {|k, v| ret[ k]||= v }
			ret
		end

		def new *vals
			ret= super *vals
			STDEXP.each {|k, v| ret[ k]||= v }
			ret
		end
	end

	def to_r exp= :main
		r = self.to_re( exp)
		#r.optimize!
		h, r = r.hidden?, r.to_r
		r = r[ 1...-1]  unless h
		::Regexp.new r
	end

	def to_re exp= :main
		u= RegExpr::Block.new
		t, u.hidden= if Symbol === exp
				u.name= exp.to_sym
				if self[ exp]
					[ self[ exp], false]
				else [ self[ exp.to_s], true]
				end
			else [ exp.to_s, true]
			end

		until !t or t.empty?
			v, t= self.to_r_next t
			case v
			when ')'  then return u, t
			when RegExpr::Repeat  then v.value= u.pop
			end
			u.push v
		end
		u
	end

	def to_r_next exp
		exp.strip!
		/^/ =~ exp[ 1.. -1]
		t= case exp[ 0]
		when ?^  then return RegExpr::Begin.new, exp[ 1.. -1]
		when ?$  then return RegExpr::End.new, exp[ 1.. -1]
		when ?\\
			h= case exp[ 1]
				when ?D, ?S, ?W, ?a, ?d.. ?f, ?n, ?r.. ?t, ?v, ?w
					return RegExpr::WildCard.new( '\%c'% exp[ 1]), exp[ 2.. -1]
				when ?x  then 16
				when ?o  then 8
				when ?b  then 2
				when ?0.. ?9
					exp= 'XX'+ exp[ 1.. -1]
					10
				else raise ArgumentError, 'Unknown form "%s"'% exp
				end
			i= exp[ 2.. -1].to_i h
			return RegExpr::Char.new( i.chr), exp[ (i.to_s( h). size+ 2).. -1]

		when ?.  then return RegExpr::WildCard.new( '.'), exp[ 1.. -1]

		when ?0
			case exp[ 1]
			when ?x  then %r<^0x([0-9a-f]+)>i.match exp
				return '', $1.to_i( 16).to_s+ $'
			when ?o  then %r<^0o([0-8]+)>.match exp
				return '', $1.to_i( 8).to_s+ $'
			when ?b  then %r<^0b([01]+)>.match exp
				return '', $1.to_i( 2).to_s+ $'
			else
				case exp
				when %r<(\d+)..(\d+)>  then RegExpr::Range.new $1.to_i, $2.to_i
				when %r<^(\d+,\d+|,\d+|\d+,?)>  then RegExpr::Repeat.new '', *$1.split( ',')
				else raise ArgumentError, 'Unknown form "%s"'% exp
				end
			end

		when ?(  then return self.to_re( exp[ 1.. -1])
		when ?)  then ')'
		when ?|  then RegExpr::Or.new

		when ?+  then RegExpr::Repeat.new '', 1, nil
		when ?*  then RegExpr::Repeat.new '', nil
		when ??  then RegExpr::Repeat.new '', 0, 1

		when ?"  then RegExpr::Char.new %r<^"((?:[^"]|\\")*)">.match( exp)[ 1]
		when ?[  then RegExpr::Chars.new %r<^\[((?:[^\]]|\\\])*[^\\]|)\]>.match( exp)[ 1]
		when ?/  then exp =~ %r<^/((?:[^/]|\\/)*)/(im?|mi)?>
			RegExpr::Regexp.new ::Regexp.new( $1,
					($2 =~ /i/ ? ::Regexp::IGNORECASE : 0)+
					($2 =~ /m/ ? ::Regexp::MULTILINE : 0))

		else
			case exp
			when %r<^([a-z_][a-z_0-9]*\b)>i  then self.to_re $1.to_sym
			when %r<(\d+)..(\d+)>  then RegExpr::Range.new $1.to_i, $2.to_i
			when %r<^(\d+,\d+|,\d+|\d+,?)>  then RegExpr::Repeat.new '', *$1.split( ',')
			else raise ArgumentError, 'Unknown form "%s"'% exp
			end
		end
		[ t, $' ]
	end

	def def cl= Class.new, *exp
		exp= [ :main ]  if exp.empty?
		exp.each do |e|
			re= self.to_re e
			names= re.names.collect('@%s'.method(:%)).join ', '
			re= ::Regexp.new '^%s$'% re.to_r
			ev= <<-EOF
				def #{e}= val
					m= #{re.inspect}. match val
					raise ArgumentError, 'Unallowed Chars! (%s =~ #{re.inspect})'% val. inspect  unless m
					#{names}= *m[ 1.. -1]
				end
			EOF
			cl.class_eval ev
		end
		cl
	end

	def match( m, exp= :main) to_r( exp).match m end

	def =~( x) to_r =~ x end
end
