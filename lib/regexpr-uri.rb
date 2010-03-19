require 'regexpr.rb'

class Uri
	class Flags< Array
		class <<self
			def new p
				self[ p. split( /[&;]/) ]
			end
		end

		def to_s
			join '&'
		end
	end

	class <<self
		def new uri
			uri. instance_of?( Uri) ? uri. dup : super( uri)
		end
	end

	attr_accessor :scheme, :username, :password, :host, :port, :path, :file, :flags, :fragment
	def initialize( uri)  self. uri= uri  end
	def uri() self. pre+ self. uri+ (self. fragment ? '#'+ self. fragment : '') end
	alias to_s uri
	def pre() (self. scheme ? self. scheme+ '://' : '')+ self. serv end
	def serv() (self. host|| '')+ (self. port ? ':'+ self. port : '') end

	def uri
		(self. path || '')+ (self. file || '')+ (self. flags ? '?'+ self. flags : '')
	end

	RegExpr = ::RegExpr[
		:scheme => '( "http" | "ftp" ) "s" ? | "sftp" | "fish"',
		:username => '[^:@]*',
		:password => '[^@]*',
		:host => 'hostname | ipv4',
		:port => 'digit +',
		:path => '( "/" ? [^?#]* "/" ) ?',
		:file => '[^/?#] *',
		:flags => '[^#] *',
		:fragment => '. *',
		'ipv4digits' => '0..255',
		'ipv6digits' => '[0-9a-bA-B] {1,4}',
		'userinfo' => 'username ( ":" password ) ?',
		'domainlabel' => 'alphadigit ( ( alphadigit | "-" ) * alphadigit ) ?',
		'hostname' => 'domainlabel ( "." | domainlabel ) *',
		'ipv4' => 'ipv4digits ( "." ipv4digits ) {3,3}',
		'request_uri' => '( path ? file ) ? ( "?" flags ) ?',
		'serv' => 'host ( ":" port ? ) ?',
		'pre' => '( ( scheme "://" ) ( auth "@" ) ? serv ) ?',
		'uri' => 'pre request_uri ( "#" fragment ) ?',
		'main' => 'uri'
	]
	RegExpr. def self, :path, :uri, :pre, :serv, :request_uri
end

def Uri( uri)  Uri.new uri  end

class String
	def to_uri() Uri. new self end
end
