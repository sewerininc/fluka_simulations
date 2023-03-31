"""
Python scrip to generate a .inp file quick such one don't wirte every grid in manual

Get the start and end of the scrip to input only geometry is created here.

everything is centered arround 0 so x_len = 20 means it goes from x=-10 to x=10
"""
output_name = "watertank"

x_len = 20  # X
width = 0.125  # Y
depth = 5.0  # Z
btw = 0.2 - width  # Y - room between each line and material
room_layer = 1  # Z
tot_layers = 1
cube_pr_layer = 17
dist_bet_layer = 2  
layers = 1
tot_vol = x_len*width*depth*cube_pr_layer
tot_vol = round(tot_vol, 1)


add_water_tank = True

material = "Brass"

start_str = """TITLE
My title
DEFAULTS                                                              HADROTHE
BEAM          -0.100    -0.001                10.0       2.5        1.PROTON
BEAMPOS          0.0       0.0      -15.       0.0       0.0
"""

end_str = f"""PHYSICS           1.                                                  COALESCE
PHYSICS           3.                                                  EVAPORAT
PHYSICS        1000.     1000.     1000.     1000.     1000.     1000.PEATHRES
IRRPROFI   15000000.      1E10
RADDECAY          1.                  3.333333303.
USRBIN         10.   ALL-PART      -41.       {round(x_len-x_len/2, 0)}    {width-width/2}         -1.xz_mid
USRBIN         -{round(x_len-x_len/2, 0)}   -{width-width/2}        -{depth+1}      400.        1.      100. &
DCYTIMES        300.      900.     3600.     7200.    14400.    28800.
DCYTIMES      57600.    86400.   115200.   259200.      450.     1400.
DCYTIMES       2500.     5400.    10800.   360000.    21600.    43200.
DCYTIMES        150.      600.
RESNUCLE          3.      -21.                      @ALLREGS     {tot_vol}cool1
RESNUCLE          3.      -22.                      @ALLREGS     {tot_vol}cool2
RESNUCLE          3.      -23.                      @ALLREGS     {tot_vol}cool3
RESNUCLE          3.      -24.                      @ALLREGS     {tot_vol}cool4
RESNUCLE          3.      -25.                      @ALLREGS     {tot_vol}cool5
RESNUCLE          3.      -26.                      @ALLREGS     {tot_vol}cool6
RESNUCLE          3.      -27.                      @ALLREGS     {tot_vol}cool7
RESNUCLE          3.      -28.                      @ALLREGS     {tot_vol}cool8
RESNUCLE          3.      -29.                      @ALLREGS     {tot_vol}cool9
RESNUCLE          3.      -30.                      @ALLREGS     {tot_vol}cool10
RESNUCLE          3.      -31.                      @ALLREGS     {tot_vol}cool11
RESNUCLE          3.      -32.                      @ALLREGS     {tot_vol}cool12
RESNUCLE          3.      -33.                      @ALLREGS     {tot_vol}cool13
RESNUCLE          3.      -34.                      @ALLREGS     {tot_vol}cool14
RESNUCLE          3.      -35.                      @ALLREGS     {tot_vol}cool15
RESNUCLE          3.      -36.                      @ALLREGS     {tot_vol}cool16
RESNUCLE          3.      -37.                      @ALLREGS     {tot_vol}cool17
RESNUCLE          3.      -38.                      @ALLREGS     {tot_vol}cool18
RESNUCLE          3.      -39.                      @ALLREGS     {tot_vol}cool19
RESNUCLE          3.      -40.                      @ALLREGS     {tot_vol}cool20
DCYSCORE          1.                         cool1     cool1          RESNUCLE
DCYSCORE          2.                         cool2     cool2          RESNUCLE
DCYSCORE          3.                         cool3     cool3          RESNUCLE
DCYSCORE          4.                         cool4     cool4          RESNUCLE
DCYSCORE          5.                         cool5     cool5          RESNUCLE
DCYSCORE          6.                         cool6     cool6          RESNUCLE
DCYSCORE          7.                         cool7     cool7          RESNUCLE
DCYSCORE          8.                         cool8     cool8          RESNUCLE
DCYSCORE          9.                         cool9     cool9          RESNUCLE
DCYSCORE         10.                        cool10    cool10          RESNUCLE
DCYSCORE         11.                        cool11    cool11          RESNUCLE
DCYSCORE         12.                        cool12    cool12          RESNUCLE
DCYSCORE         13.                        cool13    cool13          RESNUCLE
DCYSCORE         14.                        cool14    cool14          RESNUCLE
DCYSCORE         15.                        cool15    cool15          RESNUCLE
DCYSCORE         16.                        cool16    cool16          RESNUCLE
DCYSCORE         17.                        cool17    cool17          RESNUCLE
DCYSCORE         18.                        cool18    cool18          RESNUCLE
DCYSCORE         19.                        cool19    cool19          RESNUCLE
DCYSCORE         20.                        cool20    cool20          RESNUCLE
RANDOMIZ          1.
START       2000000.
STOP
"""
water = 0
if add_water_tank:
    water = 1

bb_n_void = """GEOBEGIN                                                              COMBNAME
    0    0
SPH blkbody   0.0 0.0 0.0 10000.
SPH void      0.0 0.0 0.0 9900.
"""

body = start_str + bb_n_void
unit = width + btw 
void_area = f"""BLKBODY     {cube_pr_layer*layers + water} +blkbody -void
VOID        {cube_pr_layer*layers + water} +void"""
target_str = "\n"
mat_str = f"""MATERIAL         30.     65.38      7.13                              Zink
LOW-MAT         Zink       30.       -2.      296.                    ZINC
MATERIAL                            8.73                              Brass
COMPOUND        0.58    COPPER      0.39      Zink      0.03      LEADBrass
ASSIGNMA    BLCKHOLE   BLKBODY
ASSIGNMA         AIR      VOID
"""



if add_water_tank:
    body += f"RPP target1   -50 50 -50 50 0.0 20\n"
    void_area += f" -target1"
    target_str += f"TARGET1     {cube_pr_layer*layers+1} +target1\n"
    mat_str += f"ASSIGNMA       WATER   TARGET1\n"

x_s = ""
if material == "Brass":
    x_s = "   "


for z, nr in zip(range(layers), [i*cube_pr_layer + water for i in range(layers)]):
    for i in range(1 , cube_pr_layer+1):
        y_min = round(-unit*cube_pr_layer/2 + btw*(i-1) + width*(i-1) + btw/2, 3)
        y_max = round(-unit*cube_pr_layer/2 + width + btw*(i-1)+ width*(i-1) + btw/2, 3)
        body += f"RPP target{i + nr}   {x_len/2-x_len} {x_len/2} {y_min} {y_max} {-6-(depth+dist_bet_layer)*z} {-6-(depth+dist_bet_layer)*z+depth}\n"
        void_area += f" -target{i + nr}"
        target_str += f"TARGET{i + nr}     {cube_pr_layer*layers+water} +target{i + nr}\n"
        if i + nr < 10:
            mat_str += f"ASSIGNMA    {material}  {x_s} TARGET{i + nr}\n"
        elif i + nr < 100:
            mat_str += f"ASSIGNMA    {material} {x_s} TARGET{i + nr}\n"
        else: 
            mat_str += f"ASSIGNMA    {material} {x_s}TARGET{i + nr}\n"
    
    
body += "END\n"
body += void_area
body += target_str
body +="""END
GEOEND
"""
body += mat_str
body += end_str



f = open(f"{output_name}.inp", "w")
f.write(body)
f.close()



