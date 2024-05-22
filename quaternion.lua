local Quaternion = {}
Quaternion.mt = {}
local function test(name, ...)
  return ...
end
local function number_3f(x)
  return (type(x) == "number")
end
Quaternion.mt.__eq = function(_1_, _3_)
  local _arg_2_ = _1_
  local t = _arg_2_[1]
  local x = _arg_2_[2]
  local y = _arg_2_[3]
  local z = _arg_2_[4]
  local _arg_4_ = _3_
  local T = _arg_4_[1]
  local X = _arg_4_[2]
  local Y = _arg_4_[3]
  local Z = _arg_4_[4]
  return ((t == T) and (x == X) and (y == Y) and (z == Z))
end
local function with_mt(q, mt)
  setmetatable(q, mt)
  return q
end
Quaternion.new = function(_5_)
  local _arg_6_ = _5_
  local t = _arg_6_[1]
  local x = _arg_6_[2]
  local y = _arg_6_[3]
  local z = _arg_6_[4]
  return with_mt({t, x, y, z}, Quaternion.mt)
end
local function quat(t, x, y, z)
  local t0 = (t or 0)
  local x0 = (x or 0)
  local y0 = (y or 0)
  local z0 = (z or 0)
  return Quaternion.new({t0, x0, y0, z0})
end
Quaternion.quat = quat
test("The nullary quat is the zero quaterinon", assert((quat() == quat(0, 0, 0, 0))))
local function quat_3f(z)
  return (getmetatable(z) == Quaternion.mt)
end
Quaternion["quat?"] = quat_3f
test("quat constructs quaternions (quat?=true)", assert(quat_3f(quat(1, 0, 0, 0))))
test("scalars are not quaternions", assert(not quat_3f(0)))
test("strings are not quaternions", assert(not quat_3f("x")))
local function __3equat(z)
  if quat_3f(z) then
    return z
  elseif number_3f(z) then
    return Quaternion.new({z, 0, 0, 0})
  else
    return error(":C")
  end
end
Quaternion["->quat"] = __3equat
test("->quat does nothing if the argument is already a quaternion", assert((__3equat(quat(1, 0, 0, 0)) == quat(1, 0, 0, 0))))
local function _3d_3d(u, v)
  local u0
  if number_3f(u) then
    u0 = quat(u)
  else
    u0 = u
  end
  local v0
  if number_3f(v) then
    v0 = quat(v)
  else
    v0 = v
  end
  return (u0 == v0)
end
Quaternion["=="] = _3d_3d
test("a number is == to its image in the quaternions under inclusion", assert(_3d_3d(0, quat())), assert(_3d_3d(1, quat(1))))
Quaternion.mt.__add = function(a, b)
  local _let_10_ = __3equat(a)
  local t = _let_10_[1]
  local x = _let_10_[2]
  local y = _let_10_[3]
  local z = _let_10_[4]
  local _let_11_ = __3equat(b)
  local T = _let_11_[1]
  local X = _let_11_[2]
  local Y = _let_11_[3]
  local Z = _let_11_[4]
  return quat((t + T), (x + X), (y + Y), (z + Z))
end
test("quaternions add componentwise", assert(((quat(1, 0, 0, 0) + quat(0, 1, 0, 0)) == quat(1, 1, 0, 0))))
test("addition promotes numbers to quaternions", assert((function(_12_,_13_,_14_) return (_12_ == _13_) and (_13_ == _14_) end)(quat(2),(quat(1) + 1),(1 + quat(1)))))
Quaternion.mt.__unm = function(_15_)
  local _arg_16_ = _15_
  local t = _arg_16_[1]
  local x = _arg_16_[2]
  local y = _arg_16_[3]
  local z = _arg_16_[4]
  return quat(( - t), ( - x), ( - y), ( - z))
end
local function q_plus_neg_q_is_0(q)
  return assert(((q + ( - q)) == quat()))
end
test("unary minus is the additive inverse", q_plus_neg_q_is_0, q_plus_neg_q_is_0(quat(1)), q_plus_neg_q_is_0(quat(1, 2, 3, 4)))
Quaternion.mt.__sub = function(a, b)
  return (__3equat(a) + __3equat(( - b)))
end
local function scale(k, q)
  local _let_17_ = __3equat(q)
  local t = _let_17_[1]
  local x = _let_17_[2]
  local y = _let_17_[3]
  local z = _let_17_[4]
  return quat((k * t), (k * x), (k * y), (k * z))
end
Quaternion.scale = scale
Quaternion.mt.__mul = function(q, Q)
  assert(q)
  assert(Q)
  if number_3f(q) then
    return scale(q, Q)
  elseif number_3f(Q) then
    return scale(Q, q)
  else
    local _let_18_ = q
    local t = _let_18_[1]
    local x = _let_18_[2]
    local y = _let_18_[3]
    local z = _let_18_[4]
    local _let_19_ = Q
    local T = _let_19_[1]
    local X = _let_19_[2]
    local Y = _let_19_[3]
    local Z = _let_19_[4]
    return quat(((t * T) - (x * X) - (y * Y) - (z * Z)), ((t * X) + (x * T) + (y * Z) + ( - (z * Y))), ((t * Y) + (y * T) + ( - (x * Z)) + (z * X)), ((t * Z) + (z * T) + (x * Y) + ( - (y * X))))
  end
end
local function conj(q)
  local _let_21_ = __3equat(q)
  local t = _let_21_[1]
  local x = _let_21_[2]
  local y = _let_21_[3]
  local z = _let_21_[4]
  return quat(t, ( - x), ( - y), ( - z))
end
Quaternion.conj = conj
test("conjugation negates the vector part", assert((conj(quat(0, 1)) == quat(0, -1))), assert((conj(quat(0, 1, 2, 3)) == quat(0, -1, -2, -3))), assert((conj(quat(4, 1, 2, 3)) == quat(4, -1, -2, -3))))
test("conjugation fixes 'real' quaternions", assert(_3d_3d(conj(quat(1)), 1)))
local function sqr(x)
  return (x * x)
end
local function abs2(q)
  local _let_22_ = __3equat(q)
  local t = _let_22_[1]
  local x = _let_22_[2]
  local y = _let_22_[3]
  local z = _let_22_[4]
  return (sqr(t) + sqr(x) + sqr(y) + sqr(z))
end
Quaternion.abs2 = abs2
local function abs(q)
  return math.sqrt(abs2(q))
end
Quaternion.abs = abs
Quaternion.inverse = function(z)
  local R = abs2(z)
  return scale((1 / R), conj(z))
end
Quaternion.mt.__div = function(u, v)
  if number_3f(v) then
    return scale((1 / v), u)
  else
    return (__3equat(u) * Quaternion.inverse(__3equat(v)))
  end
end
local function q_times_inv_q_is_1(q)
  return assert(((q * (1 / q)) == quat(1)))
end
test("(/ q) is the multiplicative inverse", q_times_inv_q_is_1, q_times_inv_q_is_1(quat(1)), q_times_inv_q_is_1(quat(1, 2, 3, 4)))
test("(/ i) = -i, (/ j)= -j, (/ k) = -k", assert(((1 / quat(0, 1)) == quat(0, -1))), assert(((1 / quat(0, 0, 1)) == quat(0, 0, -1))), assert(((1 / quat(0, 0, 0, 1)) == quat(0, 0, 0, -1))))
test("(/ _) is equivalent to ordinary inverse on 'real' quaternions", assert(((1 / quat(2)) == quat(0.5))))
Quaternion.i = quat(0, 1)
Quaternion.j = quat(0, 0, 1)
Quaternion.k = quat(0, 0, 0, 1)
test("i\194\178=j\194\178=k\194\178=ijk=-1", assert(_3d_3d((Quaternion.i * Quaternion.i), -1)), assert(_3d_3d((Quaternion.j * Quaternion.j), -1)), assert(_3d_3d((Quaternion.k * Quaternion.k), -1)), assert(_3d_3d((Quaternion.i * Quaternion.j * Quaternion.k), -1)))
local function __3escalar(q)
  local _let_24_ = __3equat(q)
  local t = _let_24_[1]
  local _ = _let_24_[2]
  local _0 = _let_24_[3]
  local _1 = _let_24_[4]
  return quat(t)
end
Quaternion["->scalar"] = __3escalar
local function __3evector(q)
  local _let_25_ = __3equat(q)
  local _ = _let_25_[1]
  local x = _let_25_[2]
  local y = _let_25_[3]
  local z = _let_25_[4]
  return quat(0, x, y, z)
end
Quaternion["->vector"] = __3evector
local function vec(x, y, z)
  return quat(0, x, y, z)
end
Quaternion.vec = vec
local function realpart(_26_)
  local _arg_27_ = _26_
  local r = _arg_27_[1]
  local _ = _arg_27_[2]
  local _0 = _arg_27_[3]
  local _1 = _arg_27_[4]
  return r
end
Quaternion.realpart = realpart
local function realquat_3f(q)
  local function _29_()
    local _let_28_ = q
    local t = _let_28_[1]
    local x = _let_28_[2]
    local y = _let_28_[3]
    local z = _let_28_[4]
    return ((0 == x) and (x == y) and (y == z))
  end
  return (quat_3f(q) and _29_())
end
Quaternion["realquat?"] = realquat_3f
local function real_3f(q)
  return (number_3f(q) or realquat_3f(q))
end
Quaternion["real?"] = real_3f
local function vec_3f(q)
  return (quat_3f(q) and (realpart(q) == 0))
end
Quaternion["vec?"] = vec_3f
local function cross(u, v)
  return __3evector((u * v))
end
Quaternion.cross = cross
local function dot(u, v)
  return ( - realpart((u * v)))
end
Quaternion.dot = dot
local function complex(x, y)
  return quat(x, y)
end
Quaternion.complex = complex
local function exp(q, terms)
  local terms0 = (terms or 30)
  local function loop(sum, n, qn, n_21)
    if (n == terms0) then
      return sum
    else
      return loop(((qn / n_21) + sum), (n + 1), (q * qn), ((n + 1) * n_21))
    end
  end
  return loop(0, 0, 1, 1)
end
Quaternion.exp = exp
local function _e2_89_88(x, y)
  return (abs2((x - y)) < 1e-06)
end
test("e\226\129\176 = 1", assert(_3d_3d(exp(quat(0)), 1)))
test("exp(i\207\128)=-1", assert(_e2_89_88(exp(quat(0, math.pi)), quat(-1))))
local function hopf(q, k)
  local k0 = (k or Quaternion.k)
  return (q * k0 * (1 / q))
end
Quaternion.hopf = hopf
local function stereo1(q, _ce_b5)
  local _ce_b50 = (_ce_b5 or 0.001)
  local q0 = __3equat(q)
  local _let_31_ = q0
  local t = _let_31_[1]
  local x = _let_31_[2]
  local y = _let_31_[3]
  local z = _let_31_[4]
  local shrink = (1 / ((1 + _ce_b50) - t))
  return scale(shrink, vec(x, y, z))
end
Quaternion.stereo1 = stereo1
local function stereok(q, _ce_b5)
  local q0 = __3equat(q)
  local _let_32_ = q0
  local t = _let_32_[1]
  local x = _let_32_[2]
  local y = _let_32_[3]
  local z = _let_32_[4]
  return stereo1(quat(z, x, y, t), _ce_b5)
end
Quaternion.stereok = stereok
local function tikzprint(v)
  local _let_33_ = v
  local _ = _let_33_[1]
  local x = _let_33_[2]
  local y = _let_33_[3]
  local z = _let_33_[4]
  return tex.print((x .. ", " .. y .. "," .. z))
end
Quaternion.tikzprint = tikzprint
local function G(_ce_b1)
  local function _34_(s, t)
    return ((math.cos(_ce_b1) * exp((s * Quaternion.k))) + (math.sin(_ce_b1) * exp(( - (t * Quaternion.k))) * Quaternion.j))
  end
  return _34_
end
Quaternion.G = G
local function draw(a, s, t)
  return tikzprint(stereo1((Quaternion.k * G(a)(s, t))))
end
Quaternion.draw = draw
local function drawhopf(a, s, t)
  return tikzprint(stereo1(hopf(G(a)(s, t))))
end
Quaternion.drawhopf = drawhopf
return Quaternion
