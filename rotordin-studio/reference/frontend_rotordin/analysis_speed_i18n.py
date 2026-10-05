from __future__ import annotations

"""PT/EN presentation strings for the operating/analysis-speed contract."""


_installed = False


_PT_EN = {
    "rpm nominal": "nominal rpm",
    "rpm op. min": "min operating rpm",
    "rpm op. max": "max operating rpm",
    "Campbell início [rpm]": "Campbell start [rpm]",
    "Campbell fim [rpm]": "Campbell end [rpm]",
    "Resposta início [rpm]": "Response start [rpm]",
    "Resposta fim [rpm]": "Response end [rpm]",
    "Órbita / TIMSPD [rpm]": "Orbit / TIMSPD [rpm]",
    "Discretização / discos": "Discretization / disks",
    "Camp. / d. log": "Campbell / log. dec.",
    "Desbalanceamento": "Unbalance",
    "Linha elástica": "Elastic line",
    "Mapa rigidez": "Stiffness map",
    "Gráficos": "Plots",
    "Densidade eixo [kg/m³]": "Shaft density [kg/m³]",
    "Dens. disco [kg/m³]": "Disk density [kg/m³]",
    "Div. pacote": "Stack divisions",
    "rpm ini.": "initial rpm",
    "rpm fin.": "final rpm",
    "rpm i": "initial rpm",
    "rpm f": "final rpm",
    "Div. [rpm]": "Step [rpm]",
    "Frq.": "Freq.",
    "Int.": "Interp.",
    "Amp": "Amp",
    "Inc. [°]": "Incl. [°]",
    "g [m/s²]": "g [m/s²]",
    "posi [mm]": "position [mm]",
    "ângulo [°]": "angle [°]",
    "marcador": "marker",
    "Esc.": "Scale",
    "R. Cmp": "R. Cmp",
    "Nr. Map": "No. Map",
    "Nr. Log": "No. Log",
    "Roda do mouse: zoom 2D · arraste: deslocar horizontal/vertical · Ajustar: geometria completa":
        "Mouse wheel: zoom · drag: pan",
    "Rotação nominal da máquina; não define a faixa de Campbell ou de resposta.":
        "Machine nominal speed; it does not define the Campbell or response range.",
    "Limite inferior da faixa operacional da máquina.":
        "Lower limit of the machine operating range.",
    "Limite superior da faixa operacional da máquina.":
        "Upper limit of the machine operating range.",
    "CPBIS: rotação inicial usada somente no Campbell/mapas associados.":
        "CPBIS: initial speed used only by Campbell and associated maps.",
    "CPBFS: rotação final usada somente no Campbell/mapas associados.":
        "CPBFS: final speed used only by Campbell and associated maps.",
    "UNBIF: rotação inicial da resposta dinâmica.":
        "UNBIF: initial speed of the dynamic response.",
    "UNBFF: rotação final da resposta dinâmica.":
        "UNBFF: final speed of the dynamic response.",
    "TIMSPD: rotação única da órbita. Se zero, o frontend usa rpm nominal quando ela for válida.":
        "TIMSPD: single orbit speed. If zero, the frontend uses nominal rpm when valid.",
}


def install_analysis_speed_i18n() -> None:
    global _installed
    if _installed:
        return
    from . import i18n

    i18n._PT_EN.update(_PT_EN)
    i18n._EN_PT.update({english: portuguese for portuguese, english in _PT_EN.items()})
    _installed = True


__all__ = ["install_analysis_speed_i18n"]
