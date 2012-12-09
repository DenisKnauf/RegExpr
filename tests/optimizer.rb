class OptimizerTest < Test::Unit::TestCase
	context "Optimizer" do
	should "optimize many charblocks to one charblock" do
		/[a-dt-z]/.to_s == RegExpr[ main: '[a-d] | [t-z]'].to_r.to_s
	end
end
