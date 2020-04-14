MODULE HYPELIB

    USE STATETYPE_MODULE, ONLY:     snowicestatetype,                     &
                                    soilstatetype,                        &
                                    aquiferstatetype,                     &
                                    riverstatetype,                       &
                                    lakestatetype,                        &
                                    miscstatetype
    USE WORLDVAR, ONLY :            maxcharpath
    USE LIBDATE, ONLY :             DateType, OPERATOR(.EQ.)
    USE HYPEVARIABLES, ONLY :       o_cout, o_rout, o_tobs, o_ctmp, o_prec, &
                                           o_cprc, o_evap, o_crun, o_soim

    IMPLICIT NONE
    
    ! Parameter declarations
    INTEGER, PARAMETER ::           maxoutstates = 10     !Max number of dates for saving state
    INTEGER, PARAMETER ::           logstream = 6

    ! Variable declarations
    TYPE(DateType)                  d                           !Current time
    INTEGER                         idt,        &               !Current timestep
                                    ivar,       &               !Current output variable
                                    iens,       &               !Current ensemble being simulated
                                    iout                        !Current output
!   INTEGER nobutton                                            !No exit window
    LOGICAL ::                      pwrite                      !Flag for periodend, time to write to file
    CHARACTER(LEN=maxcharpath+25)   filename                    !hyss filename
    CHARACTER(LEN=8)  ::            logdate                     !Date for log-file name
    CHARACTER(LEN=10) ::            logtime                     !Time for log-file name
    CHARACTER(LEN=3)  ::            logseq                      !Seqnr for log-file name
    INTEGER ::                      datim(8),   &               !Date in YYYYmmdd
                                    oldyear                     !year of last time step

    REAL, ALLOCATABLE ::            par(:)
    REAL                            optcrit,    & 
                                    condcrit,   &
                                    condthres
    REAL, ALLOCATABLE ::            basincrit(:,:,:)            !R2, CC, RE, RSDE, QC, QR, STDC, STDR, MAE, RMSE, Bias, STDbias, KGE, KGEpartSTD, KGEpartMM, NRMSE per subbasin och kriterie
    REAL, ALLOCATABLE ::            simperformance(:,:)         !rr2,sr2,wr2,rmae,sbias,rrve,wrve,rra,sra,meanRA,tau,medianr2,medianra,meanrs,meancc,mediankg,meanabsre
    INTEGER, ALLOCATABLE ::         subincrit(:)                !Subbasins to be included in criteria calculations
    INTEGER ::                      npar,       &               !Number of parameters to be calibrated (couted in file)
                                    nmapperiod, &               !Number of periods for map print out
                                    numoutstates                !Number of dates for saving state
    LOGICAL ::                      stateinput                  !Code for reading state
    TYPE(DateType) ::               stateoutadate(maxoutstates)       !Dates for saving state
    TYPE(DateType) ::               prestateoutdate(maxoutstates)     !Day before date for saving state (or 0)
      
    ! Variables for updating of Q and W
    LOGICAL ::                      quseobsallstations,         &
                                    quseobsnostations,          &
                                    qarnostations,              & 
                                    warnostations,              &
                                    wendupdallstations,         & 
                                    wendupdnostations
    CHARACTER(LEN=4) ::             wobsvarname
      
    ! Model state variables and other saved variables declaration
    TYPE(SNOWICESTATETYPE) ::       frozenstate
    TYPE(SOILSTATETYPE)    ::       soilstate      
    TYPE(AQUIFERSTATETYPE) ::       aquiferstate      
    TYPE(RIVERSTATETYPE)   ::       riverstate      
    TYPE(LAKESTATETYPE)    ::       lakestate
    TYPE(MISCSTATETYPE)    ::       miscstate

    INTEGER, PARAMETER :: o_fields(9) = (/ o_cout, o_rout, o_tobs, o_ctmp, o_prec, &
                                           o_cprc, o_evap, o_crun, o_soim /)

CONTAINS

    FUNCTION get_hype_time() RESULT(ret) bind(c, name="get_hype_time")

        USE, INTRINSIC :: ISO_C_BINDING
        USE MODVAR,       ONLY : currentdate

        IMPLICIT NONE

        REAL(KIND=C_FLOAT)      :: ret

        ret = convert_hype_time(currentdate)

    END FUNCTION get_hype_time

    FUNCTION get_end_time() RESULT(ret) bind(c, name="get_hype_end_time")

        USE, INTRINSIC :: ISO_C_BINDING
        USE WORLDVAR,       ONLY : sdate

        IMPLICIT NONE

        REAL(KIND=C_FLOAT)      :: ret

        ret = convert_hype_time(sdate)

    END FUNCTION get_end_time

    FUNCTION get_time_step() RESULT(ret) bind(c, name="get_hype_time_step")

        USE, INTRINSIC :: ISO_C_BINDING
        USE WORLDVAR, ONLY : steplen

        IMPLICIT NONE

        REAL(KIND=C_FLOAT)      :: ret

        ret = steplen%day*24. + steplen%hour + steplen%minute/60.

    END FUNCTION get_time_step

    FUNCTION convert_hype_time(idate) RESULT(ret)

        USE, INTRINSIC :: ISO_C_BINDING
        USE LIBDATE,      ONLY : datetype, juliandatetype, date2julian, julian2dhm
        USE TIMEROUTINES, ONLY : period_length
        USE WORLDVAR,     ONLY : bdate

        IMPLICIT NONE

        REAL                        :: ret
        TYPE(datetype), INTENT(IN)  :: idate
        TYPE(datetype)              :: period
        TYPE(juliandatetype)        :: julstart, julend, tmp

        julstart = date2julian(bdate)
        julend = date2julian(idate)

        tmp%head = julend%head - julstart%head
        tmp%tail = julend%tail - julstart%tail

        IF(tmp%tail < 0) THEN
            tmp%tail = tmp%tail + 1.
            tmp%head = tmp%head - 1.
        ENDIF

        period = julian2dhm(tmp)

        ret = period%day*24. + period%hour + period%minute/60.

    END FUNCTION convert_hype_time

    FUNCTION get_num_output_fields() RESULT(ret) bind(c, name="get_num_ovars")

        USE, INTRINSIC :: ISO_C_BINDING

        IMPLICIT NONE

        INTEGER(KIND=C_INT) :: ret

        ret = SIZE(o_fields)

    END FUNCTION get_num_output_fields


    SUBROUTINE get_output_name(index, dest) bind(c, name="get_ovar_name")

        USE, INTRINSIC :: ISO_C_BINDING
        USE MODVAR, ONLY : outvarid

        IMPLICIT NONE

        INTEGER(KIND=C_INT), INTENT(IN)                   :: index
        CHARACTER(KIND=C_CHAR), INTENT(OUT)               :: dest(*)
        INTEGER                                           :: j,k

        k = LEN(TRIM(outvarid(o_fields(index))%longname))
        DO j=1,k
            dest(j) = outvarid(o_fields(index))%longname(j:j)
        END DO
        dest(k+1) = C_NULL_CHAR

    END SUBROUTINE get_output_name


    SUBROUTINE get_output_units(index, dest) bind(c, name="get_ovar_units")

        USE, INTRINSIC :: ISO_C_BINDING
        USE MODVAR, ONLY : outvarid

        IMPLICIT NONE

        INTEGER(KIND=C_INT), INTENT(IN)                   :: index
        CHARACTER(KIND=C_CHAR), INTENT(OUT)               :: dest(*)
        INTEGER                                           :: j,k

        k = LEN(TRIM(outvarid(o_fields(index))%longunit))
        DO j=1,k
            dest(j) = outvarid(o_fields(index))%longunit(j:j)
        END DO
        dest(k+1) = C_NULL_CHAR

    END SUBROUTINE get_output_units


    FUNCTION get_num_subbasins() RESULT(ret) bind(c, name="get_num_basins")

        USE, INTRINSIC :: ISO_C_BINDING

        USE MODVAR, ONLY : nsub

        IMPLICIT NONE

        INTEGER(KIND=C_INT) :: ret

        ret = nsub

    END FUNCTION get_num_subbasins


    SUBROUTINE get_latlons(targetlatarr, targetlonarr) bind(c, name="get_latlons")

        USE, INTRINSIC :: ISO_C_BINDING
        USE MODVAR, ONLY : nsub, basin

        IMPLICIT NONE

        REAL(KIND=C_FLOAT), INTENT(OUT) :: targetlatarr(nsub)
        REAL(KIND=C_FLOAT), INTENT(OUT) :: targetlonarr(nsub)
        INTEGER           :: i

        DO i = 1, nsub
            targetlatarr(i) = basin(i)%latitude
            targetlonarr(i) = basin(i)%longitude
        END DO

    END SUBROUTINE get_latlons


    SUBROUTINE get_basin_field(index, targetarr) bind(c, name="get_ovar_values")

        USE MODVAR, ONLY : nsub, outvar, outvarindex
        USE, INTRINSIC :: ISO_C_BINDING

        IMPLICIT NONE

        INTEGER(KIND=C_INT), INTENT(IN) :: index
        REAL(KIND=C_DOUBLE), INTENT(OUT)   :: targetarr(nsub)
        INTEGER             :: i

        DO i  = 1, nsub
            targetarr(i) = outvar(i,outvarindex(o_fields(index)))
        END DO

    END SUBROUTINE get_basin_field


    FUNCTION initialize(dir, iseq) RESULT(istat) bind(c, name="init_hype")

        USE, INTRINSIC :: ISO_C_BINDING

        USE MODELMODULE, ONLY :         model_version_information,              &
                                        define_output_variables,                &
                                        set_modelconfig_from_parameters,        &
                                        initiate_model,                         &
                                        initiate_model_state,                   &
                                        define_model_parameters,                &
                                        load_modeldefined_input

        USE DATAMODULE, ONLY :          load_coded_info,                        &
                                        load_basindata,                         &
                                        load_cropdata,                          &
                                        load_pointsourcedata,                   &
                                        load_soilleakage_concentrations,        &
                                        load_branchdata,                        &
                                        load_aquiferdata,                       &
                                        load_glacierdata,                       &
                                        load_output_regions,                    &
                                        load_observations,                      &
                                        load_parameters,                        &
                                        load_submodel_info,                     &
                                        initiate_model_parameters,              &
                                        initiate_output_routines,               &
                                        set_model_configuration,                &
                                        set_model_base_configuration,           &
                                        reform_inputdata_for_submodel,          &
                                        calculate_path,                         &
                                        set_outvar,                             &
                                        set_outvar_crit,                        &
                                        prepare_for_update,                     &
                                        prepare_subbasin_output,                &
                                        prepare_outputfiles
        USE STATE_DATAMODULE, ONLY :    initiate_state_for_submodel,            &
                                        load_saved_state
        USE STATETYPE_MODULE, ONLY :    allocate_model_states
        USE WORLDVAR,    ONLY :         noutput,                                &
                                        nacrit,                                 &
                                        nsubCrit,                               &
                                        ndt,                                    &
                                        simsequence,                            &
                                        infodir,                                &
                                        modeldir,                               &
                                        resdir,                                 &
                                        forcingdir,                             &
                                        logdir,                                 &
                                        bdate,                                  &
                                        sdate,                                  &
                                        outstartdate,                           &
                                        dtskip,                                 &
                                        maxsubass,                              &
                                        simsubmodel,                            &
                                        ibasemodel,                             &
                                        doassimilation,                         &
                                        noutreg,                                &
                                        allocate_accumulation,                  &
                                        reallocate_outvar_information,          &
                                        indatacheckonoff,                       &
                                        indatachecklevel,                       &
                                        maxperf
        USE MODVAR, ONLY :              nsub,                                   &
                                        nsub_basemodel,                         &
                                        nclass,                                 &
                                        ncrop,                                  &
                                        numsubstances,                          &
                                        naquifers,                              &
                                        max_classoutvar,                        &
                                        max_basinoutvar,                        &
                                        conduct,                                &
                                        conductxoms,                            &
                                        conductregest,                          &
                                        simulate,                               &
                                        allocate_outvar,                        &
                                        noutvar,                                &
                                        noutvarclass,                           &
                                        statesize
        USE MODEL_TEST_ROUTINES, ONLY : run_hype_setup_tests,                   &
                                        run_hype_finalize_tests,                &
                                        run_hype_tests,                         &
                                        run_hype_observation_tests

        CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL  :: dir
        INTEGER(KIND=C_INT), INTENT(IN), OPTIONAL           :: iseq
        INTEGER(KIND=C_INT)                                 :: istat
        INTEGER                                             :: ii
        LOGICAL                                             :: LENDL, LENDS

        istat = 0
        iens = 1
        idt=1
        
        CALL DATE_AND_TIME(logdate, logtime, values=datim)

        CALL model_version_information(0)

        IF(PRESENT(dir)) THEN
            LENDL = .FALSE.
            LENDS = .FALSE.
            DO ii=1,maxcharpath
                IF(dir(ii) == C_NULL_CHAR .AND. .NOT. LENDL) THEN
                    LENDL=.TRUE.
                    LENDS=(dir(ii-1) == '/')
                END IF
                IF(LENDL) THEN
                    IF(.NOT. LENDS) THEN
                        infodir(ii:ii) = '/'
                        LENDS=.TRUE.
                    ELSE
                        infodir(ii:ii) = ' '
                    END IF
                ELSE
                    infodir(ii:ii) = dir(ii)
                END IF
            END DO
        ELSE
            CALL getcwd(infodir, istat)
        ENDIF

        IF(PRESENT(iseq)) THEN
            simsequence=iseq
        ENDIF

        WRITE(logseq, '(I3.3)') simsequence
        WRITE(filename, '(a)') TRIM(infodir)//'hyss_'//logseq(1:3)//'_'//logdate(3:8)//'_'//logtime(1:4)//'.log'
        OPEN(UNIT=logstream,FILE=TRIM(filename),STATUS = 'replace',ACTION='write')
        WRITE(logstream, '(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')                                   &
                 'Job start date: ',datim(1),'-',datim(2),'-',datim(3),                                 &
                 'time: ',datim(5),':',datim(6),':',datim(7)
        WRITE(logstream, *) '---------------------------------------------------'
        !nobutton = SETEXITQQ(qwin$exitnopersist)    !nobutton version
        CALL model_version_information(logstream)   !Print model version information in logg-file
        
        CALL define_output_variables()
        
        CALL define_model_parameters()

        CALL load_coded_info(infodir, istat, bdate, sdate, outstartdate, dtskip, ndt, numsubstances,    &
                             stateinput, maxoutstates, numoutstates, stateoutadate, prestateoutdate,    &
                             modeldir, resdir, forcingdir, logdir,                                      &
                             quseobsallstations, quseobsnostations, qarnostations, warnostations,       &
                             wendupdallstations, wendupdnostations, subincrit)
      IF(istat/=0) RETURN
      nmapperiod = 0 !initialization needed if no mapoutput

      CALL load_basindata(modeldir, forcingdir, nsub_basemodel, istat)
      IF(istat.NE.0) RETURN

      CALL load_cropdata(modeldir, ncrop, istat)
      IF(istat.NE.0) RETURN

      CALL load_pointsourcedata(modeldir, 'PointSourceData.txt', nsub_basemodel, istat) 
      IF(istat.NE.0) RETURN
      
      CALL load_soilleakage_concentrations(modeldir, nsub_basemodel, istat)
      IF(istat.NE.0) RETURN
      
      CALL load_branchdata(modeldir, istat)
      IF(istat.NE.0) RETURN
      
      CALL load_aquiferdata(modeldir, nsub_basemodel, naquifers, istat) 
      IF(istat.NE.0) RETURN

      CALL load_glacierdata(modeldir, nsub_basemodel, istat) 
      IF(istat.NE.0) RETURN

      CALL initiate_model_parameters(nsub_basemodel, istat)
      IF(istat.NE.0) RETURN

      WRITE(filename,'(a)') TRIM(logdir)//'tests_'//logseq(1:3)//'_'//logdate(3:8)//'_'//logtime(1:4)//'.log'
      CALL run_hype_setup_tests(fname=filename, onoff=indatacheckonoff, level=indatachecklevel) !testetup

      CALL run_hype_observation_tests(istat)
      IF(istat.NE.0) RETURN

      CALL load_submodel_info(infodir, simsubmodel, nsub, istat)   !allocation and initialisation of ibasemodel
      IF(istat.NE.0) RETURN

      CALL load_output_regions(modeldir, noutreg, istat)    !Prepare for outregions, read Outregions.txt
      IF(istat.NE.0) RETURN
      
      CALL load_observations(forcingdir, nsub_basemodel, bdate, sdate, ndt, istat)
      IF(istat.NE.0) RETURN
      
      CALL load_parameters(modeldir, nsub_basemodel, 'par.txt')
      
! Read base model defined input data and set base model configuration
      CALL set_model_base_configuration(nsub_basemodel, stateinput, modeldir, forcingdir,               &
                                        statesize, istat)
      IF(istat.NE.0) RETURN
      
      IF(simsubmodel) THEN
        CALL reform_inputdata_for_submodel(nsub_basemodel, nsub, ibasemodel)
      ENDIF
      CALL calculate_path(nsub)

! Read model defined input data and set model configuration
      CALL load_modeldefined_input(modeldir, forcingdir, nsub_basemodel, nsub, ibasemodel,              &
                                   bdate, sdate, conductxoms, conductregest, istat)
      IF(istat.NE.0) RETURN

      CALL set_model_configuration(conduct, simulate)

      CALL set_modelconfig_from_parameters()
      
! Test input data and validate options
      CALL run_hype_tests(istat)

      CALL run_hype_finalize_tests()
      
      IF(istat.NE.0) RETURN

      CALL prepare_for_update(modeldir, quseobsallstations, quseobsnostations,                          &
                              qarnostations, warnostations, wendupdallstations, wendupdnostations,      & 
                              nsub)

! Initialisations for memory allocation (states and output)
      CALL allocate_model_states(nsub_basemodel,statesize,conduct, frozenstate, soilstate,              &
                                 aquiferstate, riverstate, lakestate, miscstate)

      !CALL set_output_default_classgroup(noutput)
      istat = set_outvar(noutput, noutvar, noutvarclass)
      IF(istat.NE.0) RETURN

      istat = set_outvar_crit(noutvar, noutvarclass)
      IF(istat.NE.0) RETURN

      IF(.NOT.doassimilation) THEN
        CALL allocate_outvar(noutvar, noutvarclass)
        CALL reallocate_outvar_information(noutvar, noutvarclass)
        CALL allocate_accumulation(nsub, nclass, numsubstances, max_classoutvar, max_basinoutvar)
      ENDIF

! Allocate local variables
      nsubCrit = nsub
      ALLOCATE(basincrit(nsubCrit, maxsubass, nacrit))
      ALLOCATE(simperformance(maxperf, nacrit))

! Preparations for subbasin output
      CALL prepare_subbasin_output(subincrit, istat)
      IF(istat.NE.0) RETURN
      
      CALL DATE_AND_TIME(values=datim)
      WRITE(logstream,*)
      WRITE(logstream,*) '---------------------------------------------------'
      WRITE(logstream,'(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')                                      &
          ' Initialisations finished, calculations starts: ',datim(1),'-',datim(2),'-',datim(3),        &
          '  time: ',datim(5),':',datim(6),':',datim(7)
      WRITE(logstream,*) '---------------------------------------------------'
      FLUSH logstream

! Simulation start
! Initial model calculations; initial states, parameters
      CALL prepare_outputfiles(resdir, nsub, naquifers, 1, .FALSE., .FALSE.)
      CALL initiate_output_routines()     !All output accumulation variables zeroed
      CALL set_modelconfig_from_parameters()
      IF(simsubmodel)THEN
          CALL initiate_state_for_submodel(forcingdir, ibasemodel, stateinput, frozenstate, soilstate,  &
                                           aquiferstate, riverstate, lakestate, miscstate)
      ELSE
          IF(stateinput) THEN
              WRITE(logstream,*) 'Loading saved state.'
              CALL load_saved_state(forcingdir, nsub, frozenstate, soilstate, aquiferstate, riverstate, &
                                    lakestate, miscstate)
          ELSE
              CALL initiate_model_state(frozenstate, soilstate, aquiferstate, riverstate, lakestate, miscstate)
          ENDIF
          CALL initiate_model(frozenstate, soilstate, aquiferstate, riverstate, lakestate, miscstate) 
      ENDIF
      oldyear = 0

      istat=update()

    END FUNCTION initialize


    FUNCTION update() RESULT(istat) bind(c, name="update_hype")

        USE, INTRINSIC :: ISO_C_BINDING
        
        USE MODELMODULE, ONLY :             model
        USE DATAMODULE, ONLY :              get_current_forcing,                    &
                                            get_current_pointsources,               &
                                            initiate_outvar,                        &
                                            revise_outvar,                          &
                                            write_subbasinfiles,                    &
                                            write_subbasinfiles_class,              &
                                            write_timefiles,                        &
                                            write_timefiles_class,                  &
                                            write_regionfiles,                      &
                                            save_loadfiles
        USE WORLDVAR,    ONLY :             writematlab,                            &
                                            writeload,                              &
                                            output,                                 &
                                            noutput,                                &
                                            ndt,                                    &
                                            modeldir,                               &
                                            resdir,                                 &
                                            simsubmodel,                            &
                                            psdates
        USE MODVAR, ONLY :                  nsub,conductwarning,currentdate
        USE COMPOUT, ONLY :                 compute_mapoutput,                      &
                                            compute_outloads,                       &
                                            prepare_to_compute_crit
        USE TIMEROUTINES, ONLY:             calculate_time_for_model
        USE READWRITE_ROUTINES, ONLY:       log_progress
        USE STATE_DATAMODULE, ONLY :        finalize_outstate

        INTEGER(KIND=C_INT) :: istat
        INTEGER             :: istart
        
        istat = 0
        istart = idt
        idt = max(1, idt)

        CALL get_current_forcing(idt, nsub, d)
        CALL calculate_time_for_model(idt, d)
        CALL log_progress(oldyear, currentdate%year)
        
        IF(ALLOCATED(psdates)) CALL get_current_pointsources(modeldir, 'PointSourceData.txt', nsub, d, istat) 
        IF(istat.NE.0) RETURN

        CALL initiate_outvar(idt)
        CALL model(frozenstate, soilstate, aquiferstate, riverstate, lakestate, miscstate)

        DO ivar = 1, numoutstates    ! Write state output
            IF (d.EQ.prestateoutdate(ivar)) THEN
                IF(simsubmodel) THEN
                    IF(conductwarning) WRITE(logstream, *) 'WARNING: State can not be saved when submodel is simulated'
                ELSE
                    CALL finalize_outstate(resdir,nsub,stateoutadate(ivar), frozenstate, soilstate, aquiferstate, &
                                         & riverstate, lakestate, miscstate) 
                ENDIF
            ENDIF
        ENDDO

        CALL revise_outvar()        ! Calculate regional outvar and upstream and classes
        CALL prepare_to_compute_crit(d, idt, ndt)

        DO iout = 1, noutput
            IF(output(iout)%fileformat==6) CALL write_subbasinfiles_class(iout, idt, ndt, iens, d)
            IF(output(iout)%fileformat==5) CALL write_timefiles_class(iout, idt, ndt, iens, d)
            IF(output(iout)%fileformat==4) CALL write_regionfiles(iout, idt, ndt, iens, d)
            IF(output(iout)%fileformat==1) CALL write_subbasinfiles(iout, idt, ndt, iens, d)
            IF(output(iout)%fileformat==3) CALL write_timefiles(iout, idt, ndt, iens, d)
            IF(output(iout)%fileformat==2) CALL compute_mapoutput(d, iout, idt, ndt, writematlab, nmapperiod)  !Save data for map output
        ENDDO
        IF(writeload) THEN
            CALL compute_outloads(d, pwrite, idt, ndt)       !Write yearly load total for all subbasins
            IF(pwrite) CALL save_loadfiles(resdir, currentdate%year)
        ENDIF

        IF(istart>0) THEN
            idt = idt + 1
        ENDIF

    END FUNCTION update

    FUNCTION finalize() RESULT(istat) bind(c, name="finalize_hype")

        USE, INTRINSIC :: ISO_C_BINDING
        
        USE COMPOUT, ONLY  :                calculate_criteria
        USE DATAMODULE, ONLY :              write_simulation_assessment,                & 
                                            write_subbasin_assessment,                  &
                                            close_outputfiles,                          &
                                            close_observations,                         &
                                            save_mapfiles
        USE WORLDVAR, ONLY :                nacrit,                                     &
                                            nsubCrit,                                   &
                                            resdir,                                     &
                                            forcingdir,                                 &
                                            deallocate_worldvar,                        &
                                            optim
        USE MODVAR, ONLY:                   deallocate_modvar
        USE STATETYPE_MODULE, ONLY:         deallocate_model_states
        USE MODVAR, ONLY :                  nsub,                                       &
                                            naquifers
        USE STATE_DATAMODULE, ONLY :        finalize_outstate
        
        INTEGER(KIND=C_INT) :: istat

        istat=0
        IF(nacrit/=0) THEN
            CALL calculate_criteria(optcrit, basincrit, simperformance, condcrit, condthres)
            CALL write_simulation_assessment(resdir, iens, nacrit, optcrit, simperformance,                  &
                                             optim%task_runens, condcrit, condthres)
            CALL write_subbasin_assessment(resdir, nsubCrit, nacrit, basincrit, iens,                        &
                                           optim%task_runens)
        ENDIF

! Save and close files or prepare them for next ensemble member simulation
        CALL close_outputfiles(nsub,naquifers, 1)
        CALL close_observations(forcingdir)

! Write results to files
        CALL save_mapfiles(resdir, nsub, nmapperiod, iens, optim%task_runens,                                &
                           optim%task_writesim)
! Deallocate variables
        CALL deallocate_worldvar()
        CALL deallocate_modvar(nsub)
        CALL deallocate_model_states(frozenstate, soilstate, aquiferstate,                                   &
                                     riverstate, lakestate, miscstate)
        IF(ALLOCATED(basincrit)) DEALLOCATE(basincrit)
        IF(ALLOCATED(simperformance)) DEALLOCATE(simperformance)
        IF(ALLOCATED(par)) DEALLOCATE(par)

! Write stop time on log-file
        CALL DATE_AND_TIME (values=datim)
        WRITE(logstream,*)
        WRITE(logstream,*) '---------------------------------------------------'
        WRITE(logstream,'(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')                                         &
                ' Job finished date: ',datim(1),'-',datim(2),'-',datim(3),                                   &
                '  time: ',datim(5),':',datim(6),':',datim(7)
        CLOSE(logstream)

    END FUNCTION finalize


    END MODULE HYPELIB
