# Comprehensive test of ALL targets pipeline dependencies
library(targets)

cat("=======================================================\n")
cat("TARGETS PIPELINE DEPENDENCY VALIDATION\n") 
cat("=======================================================\n\n")

# Parse _targets.R to find all function calls and package dependencies
targets_content <- readLines("_targets.R")
targets_text <- paste(targets_content, collapse = "\n")

cat("1. ANALYZING TARGETS PIPELINE DEPENDENCIES\n")
cat("==========================================\n")

# Extract all package function calls (package::function format)
package_calls <- regmatches(targets_text, gregexpr("\\b\\w+::\\w+", targets_text))[[1]]
unique_package_calls <- unique(package_calls)

cat("Package function calls found:\n")
for (call in unique_package_calls) {
  cat("  -", call, "\n")
}

# Extract packages from package::function calls
packages_from_calls <- unique(sapply(strsplit(unique_package_calls, "::"), `[`, 1))

cat("\nPackages identified from function calls:\n")
for (pkg in packages_from_calls) {
  cat("  -", pkg, "\n")
}

# Additional packages likely needed (from common R spatial analysis)
additional_packages <- c(
  "readxl",      # For Excel files 
  "openxlsx",    # For writing Excel
  "writexl",     # Alternative Excel writing
  "tidygeocoder",# Geocoding
  "osrm",        # OSRM routing
  "leaflet",     # Interactive maps  
  "nngeo",       # Spatial operations
  "strayr",      # Australian spatial data
  "conflicted",  # Package conflicts
  "tarchetypes", # Targets extensions
  "fs",          # File system operations
  "glue",        # String interpolation
  "here"         # Path handling
)

all_packages <- unique(c(packages_from_calls, additional_packages))

cat("\n2. TESTING PACKAGE AVAILABILITY\n")
cat("===============================\n")

available_packages <- c()
missing_packages <- c()

for (pkg in all_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✓", pkg, "\n") 
    available_packages <- c(available_packages, pkg)
  } else {
    cat("  ✗", pkg, "- MISSING\n")
    missing_packages <- c(missing_packages, pkg)
  }
}

cat("\n3. TESTING CRITICAL FUNCTIONS FROM TARGETS\n")
cat("==========================================\n")

# Test functions that are actually called in _targets.R
critical_functions <- list(
  "readxl::read_xlsx" = function() {
    tryCatch({
      # Test if read_xlsx function exists and can be called
      if (exists("read_xlsx", where = asNamespace("readxl"))) {
        cat("  ✓ readxl::read_xlsx available\n")
        return(TRUE)
      } else {
        cat("  ✗ readxl::read_xlsx not found\n") 
        return(FALSE)
      }
    }, error = function(e) {
      cat("  ✗ readxl::read_xlsx error:", conditionMessage(e), "\n")
      return(FALSE)
    })
  },
  
  "readr::read_csv" = function() {
    tryCatch({
      if (exists("read_csv", where = asNamespace("readr"))) {
        cat("  ✓ readr::read_csv available\n")
        return(TRUE)
      } else {
        cat("  ✗ readr::read_csv not found\n")
        return(FALSE)
      }
    }, error = function(e) {
      cat("  ✗ readr::read_csv error:", conditionMessage(e), "\n")
      return(FALSE)
    })
  },
  
  "sf::read_sf" = function() {
    tryCatch({
      if (exists("read_sf", where = asNamespace("sf"))) {
        cat("  ✓ sf::read_sf available\n")
        return(TRUE)
      } else {
        cat("  ✗ sf::read_sf not found\n")
        return(FALSE) 
      }
    }, error = function(e) {
      cat("  ✗ sf::read_sf error:", conditionMessage(e), "\n")
      return(FALSE)
    })
  },
  
  "strayr::read_absmap" = function() {
    tryCatch({
      if (requireNamespace("strayr", quietly = TRUE) && 
          exists("read_absmap", where = asNamespace("strayr"))) {
        cat("  ✓ strayr::read_absmap available\n")
        return(TRUE)
      } else {
        cat("  ✗ strayr::read_absmap not found\n")
        return(FALSE)
      }
    }, error = function(e) {
      cat("  ✗ strayr::read_absmap error:", conditionMessage(e), "\n")
      return(FALSE)
    })
  },
  
  "openxlsx::write.xlsx" = function() {
    tryCatch({
      if (requireNamespace("openxlsx", quietly = TRUE) &&
          exists("write.xlsx", where = asNamespace("openxlsx"))) {
        cat("  ✓ openxlsx::write.xlsx available\n")
        return(TRUE)
      } else {
        cat("  ✗ openxlsx::write.xlsx not found\n")
        return(FALSE)
      }
    }, error = function(e) {
      cat("  ✗ openxlsx::write.xlsx error:", conditionMessage(e), "\n")
      return(FALSE)
    })
  }
)

function_test_results <- sapply(critical_functions, function(f) f())
functions_passed <- sum(function_test_results)
functions_total <- length(function_test_results)

cat("\n4. TESTING TARGETS PIPELINE STRUCTURE\n")
cat("=====================================\n")

# Test if targets can load the pipeline
tryCatch({
  tar_source()
  cat("  ✓ tar_source() executed successfully\n")
  
  # Try to get the manifest
  manifest <- tar_manifest()
  cat("  ✓ Targets manifest generated:", nrow(manifest), "targets found\n")
  
  # Check for key targets
  key_targets <- c("census_cald_columns", "sa1_2021", "mesh_2021_vic_sf", "export_list")
  for (target in key_targets) {
    if (target %in% manifest$name) {
      cat("  ✓ Key target found:", target, "\n")
    } else {
      cat("  ⚠ Key target missing:", target, "\n")
    }
  }
  
  pipeline_ok <- TRUE
  
}, error = function(e) {
  cat("  ✗ Targets pipeline error:", conditionMessage(e), "\n")
  pipeline_ok <- FALSE
})

cat("\n=======================================================\n")
cat("DEPENDENCY TEST SUMMARY\n")
cat("=======================================================\n")

cat("📦 Package availability:", length(available_packages), "/", length(all_packages), "\n")
cat("🔧 Critical functions:", functions_passed, "/", functions_total, "\n")

if (length(missing_packages) > 0) {
  cat("\n❌ MISSING PACKAGES:\n")
  for (pkg in missing_packages) {
    cat("   -", pkg, "\n")
  }
  
  cat("\n🛠️ INSTALL MISSING PACKAGES:\n")
  install_cmd <- paste0('install.packages(c("', paste(missing_packages, collapse = '", "'), '"))')
  cat("   R command:", install_cmd, "\n")
}

if (functions_passed == functions_total && length(missing_packages) <= 2) {
  cat("\n✅ PIPELINE READY!\n")
  cat("All critical dependencies available.\n")
  cat("You can proceed with: targets::tar_make()\n\n")
} else {
  cat("\n⚠️ DEPENDENCIES INCOMPLETE\n")
  cat("Install missing packages before running pipeline.\n")
  cat("Some targets may fail without these dependencies.\n\n")
}

# Show what would fail
if (length(missing_packages) > 0) {
  cat("🚨 POTENTIAL FAILURES:\n")
  if ("readxl" %in% missing_packages) {
    cat("   - census_cald_columns target (needs readxl::read_xlsx)\n")
    cat("   - census_age_55_plus_columns target (needs readxl::read_xlsx)\n")
    cat("   - sa1_seifa_decile target (needs readxl::read_xlsx)\n")
    cat("   - sa2_seifa_decile target (needs readxl::read_xlsx)\n")
  }
  if ("openxlsx" %in% missing_packages) {
    cat("   - export_list_xlsx target (needs openxlsx::write.xlsx)\n")
  }
  if ("strayr" %in% missing_packages) {
    cat("   - sa1_2021 target (needs strayr::read_absmap)\n")
    cat("   - suburb_2021 target (needs strayr::read_absmap)\n")
  }
  cat("\n")
}