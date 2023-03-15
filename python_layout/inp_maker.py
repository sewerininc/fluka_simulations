"""
Python scrip to generate a .inp file quick such one don't wirte every grid in manual

Get the start and end of the scrip to input only geometry is created here.

everything is centered arround 0 so x_len = 20 means it goes from x=-10 to x=10
"""
output_name = "brass_totsim"

x_len = 20  # X
width = 0.2  # Y
depth = 3.0  # Z
btw = 0.1  # Y - room between each line og material
room_layer = 1  # Z
tot_layers = 2
cube_pr_layer = 45
dist_bet_layer = 2  
layers = 2

add_water_tank = True

material = "Brass"

start_str = """TITLE
My title
DEFAULTS                                                              HADROTHE
BEAM          -0.100                          20.0      10.0        1.PROTON
BEAMPOS          0.0       0.0      -15.       0.0       0.0
"""

end_str = """PHYSICS           1.                                                  COALESCE
PHYSICS           3.                                                  EVAPORAT
PHYSICS        1000.     1000.     1000.     1000.     1000.     1000.PEATHRES
IRRPROFI   15000000.      1E10
RADDECAY          1.                  3.333333303.
DCYTIMES      86400.
RESNUCLE          3.      -21.                      @ALLREGS          cool
DCYSCORE          1.                          cool      cool          RESNUCLE
RESNUCLE          3.      -22.                      @ALLREGS          no_cool
USRBIN           10.      DOSE      -25.       10.        7.       0.0bq_gZY
USRBIN          -10.       -7.       -3.        1.      100.       30. &
USRBIN           10.      DOSE      -26.       10.        7.       0.0bq_gXY
USRBIN          -10.       -7.       -3.      200.      140.        1. &
USRBIN           10.      DOSE      -27.       10.        7.       0.8start_XY
USRBIN          -10.       -7.       0.0      200.      140.        1. &
USRBIN           10.      DOSE      -28.       10.        7.       7.6end_XY
USRBIN          -10.       -7.       6.8      200.      140.        1. &
USRBIN           10.      DOSE      -29.       10.        7.       20.dd_Z
USRBIN          -10.       -7.       0.0        1.        1.      200. &
RANDOMIZ          1.
START         20000.
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



