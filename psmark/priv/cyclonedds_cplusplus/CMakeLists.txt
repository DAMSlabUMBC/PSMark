add_library(psmark_cyclonedds SHARED)

include(FetchContent)
FetchContent_Declare(
  CycloneDDS
  GIT_REPOSITORY https://github.com/eclipse-cyclonedds/cyclonedds.git
  GIT_TAG        11.1.0
)
FetchContent_MakeAvailable(CycloneDDS)

target_sources(psmark_cyclonedds
    PRIVATE
        NIF/CycloneDDSNIF.cpp
)

# CycloneDDS exports its Generate.cmake script
# thus it is possible to directly call the IDL compiler
idlc_generate(
    TARGET psmark_cyclonedds_idl
    FILES DDS_IDL/CycloneDDSPSMark.idl
)

target_link_libraries(psmark_cyclonedds PUBLIC 
    CycloneDDS::ddsc      # DDS definitions
    psmark_cyclonedds_idl   # Compiled IDL library
)

target_include_directories(psmark_cyclonedds PRIVATE ${ERTS_ROOT}/include)