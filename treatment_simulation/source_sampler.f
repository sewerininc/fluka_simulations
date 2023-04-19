*$ CREATE SOURCE.FOR
*COPY SOURCE

!> @brief
!! Particle source for pencil beam scanning in hadrontherapy.
!! It mimics the beam coming out from gantry nozzle and aims
!! at creating spread-out Bragg peak shape in depth
!! Simulated source consists of several beamlets,
!! each of them characterized by relative weight,
!! energy and size (both defined at source location).
!! Additionally energy spread may be specified by the user.
!! User can also decide whether divergent beam is emitted from
!! a nozzle plane or point-like virtual source is used.
!! This source needs an additional file (typically sobp.dat)
!! with description of pencil beam geometry and kinematics.
!!
!! This implementation is based on a template from $FLUPRO/usermvax/source.f
!!
!! In order to use the source, first compile this file using
!! following command or Flair GUI:
!!
!!  $FLUPRO/flutil/ldpm3qmd source_sampler.f -o flukadpm3_sobp
!!
!! Then get a file called sobp.dat and put it in the same directory as
!! your Fluka input file. In the input file add a card called SOURCE
!! to activate this custom source. To run it, call (or use Flair):
!!
!! rfluka -N0 -M1 -e flukadpm3_sobp your_input_file
!!
!!
!!
!! @details
!!
!! -------------------- SOBP CONFIG FILE -------------------------------------
!! Input file (typically sobp.dat) is a text file.
!! Comment lines start with *. It may have 5,6 or 7 columns.
!! These columns can be:
!!
!!  - 5 columns: E, X, Y, FWHM, W
!!  - 6 columns: E, X, Y, FWHM_X, FWHM_Y, W
!!  - 7 columns: E, DE, X, Y, FWHM_X, FWHM_Y, W
!!
!! where:
!!
!!  - E : kinetic energy in GeV/amu
!!  - DE : energy spread (sigma) in GeV/amu
!!  - X, Y : position (in cm) of the beamlet/spot center
!!  - FWHM_X, FWHM_Y, FWHM : spot size (in cm)
!!
!! All above-mentioned quantities are defined at beam source location (typically nozzle exit)
!!
!! For more details, see SHIELD-HIT12A manual http://shieldhit.org/index.php?id=documentation
!!
!!
!!
!! -------------------- Beam configuration in the INPUT FILE -------------------------------------
!! Beam momentum/energy, its spread and divergence is usually specified in BEAM input card.
!! Also beam width is specified there.
!! All these values except momentum spread will be ignored and overridden.
!! In case 5- or 6-columns sobp.dat file format (the case where DE is missing) is used,
!! then beam momentum will be taken from the input card, otherwise is will be ignored and overriden.
!!
!! Center of the beam spot is usually defined in BEAMPOS card in the input file,
!! which contains X,Y,Z positions and direction cosines.
!! Whatever is specified as X,Y and direction cosines will be ignored and overridden
!! by this custom source. SDUM value in BEAMPOS card will also be ignored.
!!
!! In order to activate this custom source, please add SOURCE card to the input file.
!! It has one optional parameter: WHAT(1), which (if present) will be understood
!! as the position of the virtual source. We assume virtual source is located on
!! negative part of Z axis, thus this position is given as single negative number.
!! Another parameter (so called SDUM) is the filename containing table of numbers
!! with beam specification. Typically its called sobp.dat.
!! An example SOURCE card looks like that:
!!
!! SOURCE           -205.0                                                sobp.dat
!!
!!
!! For more details, see FLUKA documentation:
!!  - BEAM    card http://www.fluka.org/fluka.php?id=man_onl&sub=12
!!  - BEAMPOS card http://www.fluka.org/fluka.php?id=man_onl&sub=14
!!  - SOURCE  card http://www.fluka.org/fluka.php?id=man_onl&sub=71
!!
!! -------------------------------------------------------------------------------------------------
!! ==================================================================================================
!! ==================================================================================================

!> @brief
!! Returns number of text blocks separated with white characters (spaces)
!! It corresponds to number of columns in CSV file.
!! @param[in] STRING
!! @retval LENGTH number of columns
      INTEGER FUNCTION NCOLS( ASTRING )
      IMPLICIT NONE
      CHARACTER*(*) ASTRING   ! input - string of arbitrary length
      INTEGER I               !
      NCOLS = 0

*     make a loop and check for non-space followed by space
*     each occurence will mark end of a column
      DO I = 1, LEN (ASTRING) - 1
         IF ((ASTRING (I:I) .NE. ' ') .AND.
     & (ASTRING(I+1:I+1) .EQ. ' ')) THEN
            NCOLS = NCOLS + 1
         END IF

*     in a special case when last character isn't a space, add another column
      IF ( ASTRING( LEN(ASTRING):LEN(ASTRING) ) .NE. ' ' ) THEN
        NCOLS = NCOLS + 1
      END IF

      END DO

      RETURN
      END


!! ==================================================================================================
!! ==================================================================================================

!> @brief
!! Reads beam configuration file
!! @param[in] FILEPATH  path to the beam configuration file
!! @param[out] ENERGY  particle energy in GeV/amu
!! @param[out] DE  particle energy spread (sigma) in GeV/amu
!! @param[out] XPOS beam spot center (X coordinate), in cm
!! @param[out] YPOS beam spot center (Y coordinate), in cm
!! @param[out] FWHMX beam spot size in X axis, in cm
!! @param[out] FWHMY beam spot size in Y axis, in cm
!! @param[out] PART beamlet weight
!! @param[out] NCOLUMNS number of columns in the file, negative if file missing or corrupted
!! @param[out] NWEIGHT number of data rows in the file, negative if file missing or corrupted
      SUBROUTINE READSOBP ( FILEPATH,
     $   ENERGY, DE, XPOS, YPOS, FWHMX, FWHMY, PART,
     $   NCOLUMNS, NWEIGHT )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

      CHARACTER*(*) FILEPATH   ! path to the sobp file
      CHARACTER(8192) LINE
      DOUBLE PRECISION ENERGY(65000), DELTAE(65000)
      DOUBLE PRECISION XPOS(65000), YPOS(65000)
      DOUBLE PRECISION FWHMX(65000), FWHMY(65000)
      DOUBLE PRECISION PART(65000)
      INTEGER NCOLUMNS
      LOGICAL LEXISTS

      WRITE(LUNOUT,*) 'SOBP SOURCE READING ', FILEPATH

*     warn if sobp.dat file is missing, stop calculation
      INQUIRE( FILE=FILEPATH, EXIST=LEXISTS )
      IF ( .NOT. LEXISTS ) THEN
         WRITE(LUNOUT,*) 'SOBP FILE sobp.dat missing'
         RETURN
      END IF
*
*     open sobp.dat for reading
      OPEN( 44, FILE=FILEPATH, STATUS='OLD' )
*
*     we will now probe the file to get the number of columns with numbers
*     we skip comment lines, first non-comment line will be saved to LINE
*     later we rewind the file to the beginning and read it with proper column format
      LINE(1:1) = '*'
      DO WHILE( LINE(1:1) .EQ. '*' )
         READ(44,'(A)',END=10) LINE
      END DO
      REWIND(44)
*     WRITE(LUNOUT,*) 'SOBP FIRST LINE', TRIM(LINE)
*
*     get number of columns from the first non-comment line
      NCOLUMNS = NCOLS(LINE)
      WRITE(LUNOUT,*) 'SOBP NUMBER OF COLUMNS', NCOLUMNS


      DO
*        fortran arrays start with 1, so we increase the counter as the loop starts
         NWEIGHT = NWEIGHT + 1
         IF (NWEIGHT .GT. 65000) THEN
            WRITE(LUNOUT,*) 'SOBP SOURCE ERROR: too many beamlets'
            RETURN
         ENDIF

*        skip comment lines, first non-comment line will be saved to LINE
         LINE(1:1) = '*'
         DO WHILE( LINE(1:1) .EQ. '*' )
            READ(44,'(A)',END=10) LINE
         END DO

*        read the line, and guess the format from number of columns
         IF ( NCOLUMNS .EQ. 5 ) THEN

            READ(LINE,*,END=10) ENERGY(NWEIGHT), XPOS(NWEIGHT),
     $         YPOS(NWEIGHT), FWHMX(NWEIGHT), PART(NWEIGHT)
            FWHMY = FWHMX
            DELTAE = 0.0D0

         ELSE IF ( NCOLUMNS .EQ. 6 ) THEN

            READ(LINE,*,END=10) ENERGY(NWEIGHT), XPOS(NWEIGHT),
     $         YPOS(NWEIGHT), FWHMX(NWEIGHT), FWHMY(NWEIGHT),
     $         PART(NWEIGHT)
            DELTAE = 0.0D0

         ELSE IF ( NCOLUMNS .EQ. 7 ) THEN

            READ(LINE,*,END=10) ENERGY(NWEIGHT), DELTAE(NWEIGHT),
     $         XPOS(NWEIGHT), YPOS(NWEIGHT),
     $         FWHMX(NWEIGHT), FWHMY(NWEIGHT),
     $         PART(NWEIGHT)

         ELSE
                WRITE(LUNOUT,*) 'SOBP WRONG NO OF COLUMNS',NCOLUMNS
                RETURN
         END IF

      ENDDO


 10   CONTINUE
*     fix index
	  NWEIGHT = NWEIGHT - 1
      WRITE(LUNOUT,*) 'SOBP SOURCE beamlets found:', NWEIGHT

      RETURN
      END

!! ==================================================================================================
!! ==================================================================================================

!> @brief
!! Main user source subroutine
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2010      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     New source for FLUKA9x-FLUKA20xy:                                *
*                                                                      *
*     Created on 07 January 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on  17-Oct-10    by    Alfredo Ferrari               *
*                                                                      *
*  This is just an example of a possible user written source routine.  *
*  note that the beam card still has some meaning - in the scoring the *
*  maximum momentum used in deciding the binning is taken from the     *
*  beam momentum.  Other beam card parameters are obsolete.            *
*                                                                      *
*  Useful materials on how to prepare user source can be found here:   *
*  http://www.fluka.org/content/course/NEA/lectures/UserRoutines.pdf   *
*                                                                      *
*       Output variables:                                              *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(BEAMCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(IOIOCM)'
      INCLUDE '(LTCLCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(SOURCM)'
      INCLUDE '(SUMCOU)'
      INCLUDE '(CASLIM)'
*
*
*     containers to store data from sobp.dat file
      DOUBLE PRECISION ENERGY(65000), DELTAE(65000)
      DOUBLE PRECISION XPOS(65000), YPOS(65000)
      DOUBLE PRECISION FWHMX(65000), FWHMY(65000)
      DOUBLE PRECISION PART(65000)
*
      INTEGER NWEIGHT, NCOLUMNS
      LOGICAL LPNTSRC
      LOGICAL LENMOMPOS
      DOUBLE PRECISION SRC2SPOT, AIRSCAT
      DOUBLE PRECISION FWHM2SIGMA
*
      SAVE ENERGY, DELTAE, XPOS, YPOS
      SAVE FWHMX, FWHMY, PART
      SAVE NWEIGHT
*
      LOGICAL LFIRST
*
      SAVE LFIRST
      DATA LFIRST / .TRUE. /
*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
*  |  *** The following 3 cards are mandatory ***
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.
*  |  *** User initialization ***

         FWHM2SIGMA = 2.0D0 * SQRT( 2.0D0 * LOG(2.0D0))
         WRITE(LUNOUT,*) 'SOBP SOURCE ZPOS fixed to', ZBEAM
         WRITE(LUNOUT,*) 'SOBP USER PARAM LIST', WHASOU

*        Fluka run happens in a temporary directory,
*        created in same level as input file
*        sobp.dat is not copied there,
*        We reach one level up to get it via ../sobp.dat
         CALL READSOBP ( '../sobp.dat', ENERGY, DE,
     $            XPOS, YPOS, FWHMX, FWHMY, PART, NCOLUMNS, NWEIGHT )

*        In case of problem with reading sobp.dat file
         IF ( (NCOLUMNS .LE. ZERZER) .OR. (NWEIGHT .LE. ZERZER)) THEN
            NOMORE = 1
            RETURN
         ENDIF

*        First parameter in SOURCE indicates position of virtual source.
*        If the number is present and non-zero we expect point-like source
         IF ( WHASOU(1) .NE. 0.0D0 ) THEN
            WRITE(LUNOUT,*) 'SOBP POINT-LIKE VIRTUAL SOURCE'
            LPNTSRC = .TRUE.
         ELSE
            WRITE(LUNOUT,*) 'SOBP PARALLEL VIRTUAL SOURCE'
            LPNTSRC = .FALSE.
         END IF

      END IF
*  |  End of first call initializations.
*  +-------------------------------------------------------------------*

*  +-------------------------------------------------------------------*
*  |  sample beamlet collection :


*     Get a 64-bit pseudo random number in the interval [0.D+00,1.D+00),
*     1 being not included
      RAN = FLRNDM(111)

*     Now an integer random number, from the 1 to NWEIGHT,
*     needed to select random line from sobp config file
*     http://infohost.nmt.edu/tcc/help/lang/fortran/scaling.html
*     If you want an integer between i and j inclusive
*     use int(rand(0)*(j+1-i))+i
      NRAN = INT(RAN * NWEIGHT) + 1

*     check if random number selected properly
      IF ((NRAN .GT. NWEIGHT) .OR. (NRAN .LT. 1)) THEN
         WRITE(LUNOUT,*) 'SOBP SOURCE ERROR. NRAN, RAN:', NRAN, RAN
         NOMORE = 3
         RETURN
      END IF


*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
* Npflka is the stack counter: of course any time source is called it
* must be =0
      NPFLKA = NPFLKA + 1
* Wt is the weight of the particle
      WTFLK (NPFLKA) = PART(NRAN)    ! set new weight
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
* Particle type (1=proton.....). Ijbeam is the type set by the BEAM
* card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron:
      ELSE
         IONID = IJBEAM
         ILOFLK (NPFLKA) = IJBEAM
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
      END IF
*  |
*  +-------------------------------------------------------------------*
* From this point .....
* Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
* User dependent flag:
      LOUSE  (NPFLKA) = 0
*  No channeling:
      KCHFLK (NPFLKA) = 0
      ECRFLK (NPFLKA) = ZERZER
* User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
* User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
* Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
* ... to this point: don't change anything
* Particle age (s)
      AGESTK (NPFLKA) = +ZERZER
      AKNSHR (NPFLKA) = -TWOTWO
****************************************************************



*  +-------------------------------------------------------------------*
*  |  Particle momentum and energy

*      In sobp.dat file energy is saved in GeV/amu, while
*      in Fluka we need it in not per amu, but simply in GeV
*      To achieve this we multiply energy by mass number A
*      Fluka doesn't have any consistent method of calculating
*      mass number (number of nucleons) for simple particles
*      (such as protons and alphas) and heavy ions
*      we use a method IBARCH to get baryonic charge which
*      reduces to mass number for particles of interest
       ENERGY(NRAN) = ENERGY(NRAN) * IBARCH(IONID)


*  ....................................................................................
*      Basic equation which relates particle energy and momentum is:
*
*         p^2 c^2 + m0^2 c^4 = E^2                 (1)
*
*      Total energy E can be written as the sum of kinetic energy T and rest energy:
*
*         p^2 c^2 + m0^2 c^4 = (T + m0 c^2)^2      (2)
*
*      By doing simple math we can extract from this formula following relations:
*
*         T = sqrt(p^2 c^2 + m0^2 c^4) - m0 c^2    (3)
*         p c = sqrt( E (E + 2 m0 c^2) )           (4)
*
*      We use following variables below:
*        @   PMOFLK - particle momentum, in GeV/c
*        @   AM(IONID) - particle rest energy, in GeV
*        @   ENERGY(NRAN) - particle kinetic energy, in GeV
*  ....................................................................................
*
*      There is always a small chance that randomly sampled energy or momentum
*      will get negative (i.e. mean energy 10 MeV with energy spread 8 MeV).
*      We will sample gaussian distribution in a loop until positive value is obtained
*
       LENMOMPOS = .FALSE.
       DO WHILE ( LENMOMPOS .EQV. .FALSE. )

          IF ( ENERGY(NRAN) .LE. ZERZER ) THEN
             WRITE(LUNOUT,*) 'SOBP NEGATIVE EN:', ENERGY(NRAN)
             NOMORE = 4
             RETURN
          END IF

*         Lets get a normally distributed random number RGAUSS
          CALL FLNRRN(RGAUSS)

          IF ( NCOLUMNS .EQ. 7 ) THEN

*            In case sobp.dat file has 7 columns, we expect than
*            energy spread will be there, set as standard deviation
*            First lets sample gaussian energy distribution.
*            Mean energy is set to ENERGY(NRAN), value from sobp.dat file
*            Standard deviation is also taken from sobp.dat file
             TKEFLK (NPFLKA) = ENERGY(NRAN) + DELTAE(NRAN)*RGAUSS

*            Exit the loop if kinetic energy is positive (momentum will also be positive)
             IF ( TKEFLK (NPFLKA) .GT. ZERZER ) THEN
                LENMOMPOS = .TRUE.
             ELSE
                WRITE(LUNOUT,*) 'KINETIC ENERGY:', TKEFLK (NPFLKA)
                WRITE(LUNOUT,*) 'RESAMPLING TO GET POSITIVE NUMBER'
             END IF

*            Momentum of the particle, according to eq (4)
             PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA)*
     &        ( TKEFLK (NPFLKA) + TWOTWO * AM (IONID) ))

          ELSE

*            In case energy spread is not provided in sobp.dat file,
*            we can use a momentum spread from BEAM input card (which by default is set to 0).
*            Mean momentum is calculated from kinetic energy of the spot, using eq (4)
*            Standard deviation is obtained from FWHM value set by user in the input card (DPBEAM)
             PMOFLK (NPFLKA) = SQRT ( ENERGY(NRAN) *
     &          ( ENERGY(NRAN) + TWOTWO * AM (IONID) ))
     &          + DPBEAM*RGAUSS/FWHM2SIGMA

*
*            Exit the loop if momentum is positive (kinetic energy will also be positive)
             IF ( PMOFLK (NPFLKA) .GT. ZERZER ) THEN
                LENMOMPOS = .TRUE.
             ELSE
                WRITE(LUNOUT,*) 'MOMENTUM:', PMOFLK (NPFLKA)
                WRITE(LUNOUT,*) 'RESAMPLING TO GET POSITIVE NUMBER'
             END IF

*            Kinetic energy of the particle (GeV), according to eq (3)
             TKEFLK (NPFLKA) = SQRT(PMOFLK(NPFLKA)**2 + AM(IONID)**2)
     &         -AM(IONID)
*

          END IF

       END DO

*      debugging printouts only if requested by user
       IF ( WHASOU(2) .NE. 0.0D0 ) THEN
          WRITE(LUNOUT,*) 'SOBP E:', 1.D3*ENERGY(NRAN), 'MeV/amu'
          WRITE(LUNOUT,*) 'SOBP Z:', ZBEAM, 'cm'
          WRITE(LUNOUT,*) 'SOBP SOURCE EKIN', TKEFLK(NPFLKA)
          WRITE(LUNOUT,*) 'SOBP SOURCE MOMENTUM', PMOFLK(NPFLKA)
       ENDIF

*  +-------------------------------------------------------------------*
*  |  Direction cosines (tx,ty,tz)

      IF( LPNTSRC ) THEN

*        calculate distance from virtual beam source to the center of spot position
         SRC2SPOT = SQRT(ZBEAM*ZBEAM
     &       + XPOS(NRAN)*XPOS(NRAN)
     &       + YPOS(NRAN)*YPOS(NRAN))

*        we use a vector from virtual source to the spot center
*        to calculate direction cosines
         TXFLK  (NPFLKA) = XPOS(NRAN) / SRC2SPOT
         TYFLK  (NPFLKA) = YPOS(NRAN) / SRC2SPOT

      ELSE

*        parallel beam, direction cosines are (0,0,1)
         TXFLK  (NPFLKA) = ZERZER
         TYFLK  (NPFLKA) = ZERZER

      END IF

*     to ensure proper normalization last cosine (tz) is calculated
*     from the first two (tx, ty)
      TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
     &                       - TYFLK (NPFLKA)**2 )

*      debugging printouts only if requested by user
       IF ( WHASOU(2) .NE. 0.0D0 ) THEN
          WRITE(LUNOUT,*) 'SOBP SOURCE cosine X', TXFLK  (NPFLKA)
          WRITE(LUNOUT,*) 'SOBP SOURCE cosine Y', TYFLK  (NPFLKA)
          WRITE(LUNOUT,*) 'SOBP SOURCE cosine Z', TZFLK  (NPFLKA)
       ENDIF


*  +-------------------------------------------------------------------*
*  |  Polarization cosines (TXPOL=-2 flag for "no polarization"):

      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER

*  +-------------------------------------------------------------------*
*  |  Particle coordinates

*     First goes point like source. Source is located at (0,0,ZBEAM)
*     whatever user provides as XBEAM and YBEAM is overridden with zeros
      IF( LPNTSRC ) THEN
        XBEAM = 0.0D0
        YBEAM = 0.0D0
*     Second goes parallel source. Whatever user provides as XBEAM and YBEAM is overriden.
*     For each spot center os spot positions goes to XBEAM and YBEAM.
      ELSE
        XBEAM = XPOS(NRAN)
        YBEAM = YPOS(NRAN)
      END IF

*     Now we set initial displacement (relative to the spot center)
*     Starting value -> sigma
      XSPOT = FWHMX(NRAN) / FWHM2SIGMA
      YSPOT = FWHMY(NRAN) / FWHM2SIGMA


*     sample a gaussian position
      CALL FLNRR2 (RGAUS1, RGAUS2)

*     use center position and displacement multiplied by a
*     random number sampled from gaussian distribution N(0,1)
      XFLK(NPFLKA) = XBEAM + XSPOT * RGAUS1
      YFLK(NPFLKA) = YBEAM + YSPOT * RGAUS2
      ZFLK(NPFLKA) = ZBEAM

*      debugging printouts only if requested by user
       IF ( WHASOU(2) .NE. 0.0D0 ) THEN
          WRITE(LUNOUT,*) 'SOBP XPOS:', XFLK(NPFLKA), 'cm'
          WRITE(LUNOUT,*) 'SOBP YPOS:', YFLK(NPFLKA), 'cm'
       ENDIF



*  +-------------------------------------------------------------------*
*  +-------------------------------------------------------------------*


*  Calculate the total kinetic energy of the primaries: don't change
      IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &   THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
      RADDLY (NPFLKA) = ZERZER

*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV
      RETURN
*=== End of subroutine Source =========================================*
      END
