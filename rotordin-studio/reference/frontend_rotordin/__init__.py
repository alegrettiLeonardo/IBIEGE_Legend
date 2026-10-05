"""Frontend RotorDin 100% Python; a física é delegada ao solver RotorDin."""

__version__ = "0.2.0"

# Install legacy iRdin input-translation parity before callers import the solver,
# legacy importer or Python mesh preview. The contract changes only frontend
# serialization/preview behavior; RotorDin's Fortran equations remain untouched.
from .legacy_package_contract import install_legacy_package_contract
from .analysis_speed_contract import install_analysis_speed_contract
from .legacy_calculation_contract import install_legacy_calculation_contract
from .analysis_speed_i18n import install_analysis_speed_i18n

install_legacy_package_contract()
install_analysis_speed_contract()
install_legacy_calculation_contract()
install_analysis_speed_i18n()
