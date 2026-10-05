from __future__ import annotations

"""Runtime Portuguese/English presentation layer for RotorDin.

Only presentation text is translated. Canonical/domain values remain unchanged so
switching language cannot change the Fortran input contract or a saved project.
"""

from enum import StrEnum
from typing import Any


class Language(StrEnum):
    PORTUGUESE = "pt_BR"
    ENGLISH = "en_US"


_current_language = Language.PORTUGUESE


_PT_EN: dict[str, str] = {
    # shell / menus
    "Arquivo": "File", "Novo": "New", "Abrir": "Open", "Salvar": "Save",
    "Salvar como": "Save As", "Sair": "Exit", "Gráficos": "Plots",
    "Opções": "Options", "Configurações": "Settings", "Externos": "External",
    "Ajuda": "Help", "Sobre": "About", "Idioma / Language": "Language / Idioma",
    "Português": "Portuguese", "Pronto": "Ready", "Principal": "Main",
    "Executar RotorDin": "Run RotorDin", "Resultados": "Results", "Saída": "Output",
    "Visualizar input": "View input", "Visualizar eixo": "View rotor",
    # identification / analysis
    "Identificação": "Identification", "Identificação do Rotor": "Rotor Identification",
    "Rot. Crítica": "Critical Speed", "Usuário:": "User:", "Caso:": "Case:",
    "Linha": "Line", "Polos": "Poles", "Carc.": "Frame", "Usu.": "User",
    "Data": "Date", "obs": "notes", "Análise LATERAL": "LATERAL Analysis",
    "Executar análise lateral": "Run lateral analysis", "Análise TORCIONAL": "TORSIONAL Analysis",
    "Executar análise torcional": "Run torsional analysis", "Análise AVANÇADA": "ADVANCED Analysis",
    "Modelos / análises avançadas": "Advanced models / analyses", "Campb.": "Campbell", "Campbell / D.log": "Campbell / log. dec.",
    "resp d.": "direct resp.", "modos": "modes", "linha": "line", "mapa": "map",
    "órbita": "orbit", "d. log": "log. dec.", "Resposta direta": "Direct response",
    "Resposta ao Desbalanceamento": "Unbalance Response", "Resposta": "Response",
    "Modos": "Modes", "Mapa": "Map", "Órbita": "Orbit", "D. log": "Log. dec.",
    "Frequência": "Frequency", "Harmônica": "Harmonic", "Torção estática": "Static torsion",
    "Transiente": "Transient", "Flexo-Torção": "Flexural-Torsional",
    "fundação din.": "dynamic foundation", "discos flex.": "flexible disks",
    # primary model tabs
    "Segmentos": "Segments", "Segmentos do Eixo": "Shaft Segments",
    "Mancais": "Bearings", "Massas": "Masses", "Força / Resp.": "Force / Response",
    "Forças": "Forces", "Forças e Pontos de Resposta": "Forces and Response Points",
    "Concentradas": "Concentrated", "Suporte": "Support", "Suportes": "Supports",
    "Fundação": "Foundation", "Fundação Dinâmica": "Dynamic Foundation",
    "Configurações do RotorDin": "RotorDin Settings", "Configurações — Cálculo": "Settings — Calculation",
    "Configurações — Transiente": "Settings — Transient", "Análise Transiente": "Transient Analysis",
    "Discos Flex.": "Flex. Disks", "Discos flexíveis": "Flexible disks",
    "Massas — Rígidas": "Masses — Rigid", "Massas — Flexíveis": "Masses — Flexible",
    "Massas — Concentradas": "Masses — Concentrated",
    # common forms
    "✓ Alterar": "✓ Update", "＋ Incluir": "＋ Add", "－ Excluir": "－ Delete",
    "Alterar": "Update", "Incluir": "Add", "Excluir": "Delete", "Seção": "Section",
    "Tipo de elemento": "Element type", "Tipo": "Type", "Massa": "Mass",
    "Pacote": "Stack", "Resp. Desbalanceamento": "Unbalance Response",
    "API 541…": "API 541…", "Probes nos mancais": "Bearing probes",
    "Posicionamento": "Positioning", "Legado geométrico": "Legacy geometric",
    "API/modal — forma modal": "API/modal — mode shape",
    "Modo modal": "Modal mode", "Origem modal": "Modal source",
    "Sem forma modal atual": "No current mode shape",
    "Adicionar desbalanceamento — API 541": "Add unbalance — API 541",
    "Dados de cálculo": "Calculation data",
    "Translacional / cilíndrica": "Translational / cylindrical", "Cônica": "Conical",
    "Quantidade": "Quantity", "Início da região [mm]": "Region start [mm]",
    "Comprimento da região [mm]": "Region length [mm]",
    "Rotação N [rpm]": "Speed N [rpm]",
    "Carga estática W₁ [kg]": "Static load W₁ [kg]",
    "Carga estática W₂ [kg]": "Static load W₂ [kg]",
    "Fator de resposta": "Response factor", "Fase inicial [°]": "Initial phase [°]",
    "Inserção": "Insertion", "Adicionar aos existentes": "Append to existing",
    "Substituir somente desbalanceamentos existentes": "Replace unbalance rows only",
    "Origem da região": "Region source", "Origem de W": "W source",
    "Preview": "Preview", "Atualizar preview": "Refresh preview",
    "U aplicado [g·mm]": "Applied U [g·mm]", "Solver [kg·m]": "Solver [kg·m]",
    "Fase": "Phase", "Valor": "Value", "Orient.": "Orient.", "Coord.": "Coord.",
    "Solver / material": "Solver / material", "Densidade [kg/m³]": "Density [kg/m³]",
    "Discretização / Campbell": "Discretization / Campbell", "Div. trecho cônico": "Conical segment div.",
    "Nº pontos (CPBSPD)": "No. points (CPBSPD)", "Passo Δrpm (UNBFD) [rpm]": "Step Δrpm (UNBFD) [rpm]",
    "Resposta / saída": "Response / output", "Distância:": "Distance:", "Massa:": "Mass:",
    "Início:": "Start:", "Compr.:": "Length:", "Geometria": "Geometry",
    "Propriedades": "Properties", "Seção selecionada": "Selected section",
    "Sem segmentos": "No segments", "Tipo de elemento:": "Element type:",
    "D inicial [mm]:": "Initial D [mm]:", "D final [mm]:": "Final D [mm]:",
    "D int inicial [mm]:": "Initial inner D [mm]:", "D int final [mm]:": "Final inner D [mm]:",
    "Comprimento (L):": "Length (L):", "Diâmetro inicial (D):": "Initial diameter (D):",
    "Diâmetro final (D_F):": "Final diameter (D_F):",
    "Diâmetro interno inicial:": "Initial inner diameter:", "Diâmetro interno final:": "Final inner diameter:",
    "Área inicial (A):": "Initial area (A):", "Inércia (I):": "Inertia (I):",
    "Circular": "Circular", "Costelada": "Ribbed",
    # sketch
    "Eixo": "Rotor", "Vista do Eixo": "Rotor View", "Vista do Rotor": "Rotor View",
    "Vista interativa do rotor": "Interactive rotor view", "Ajustar à janela": "Fit to window",
    "Copiar imagem": "Copy image", "Defina os segmentos do eixo": "Define the shaft segments",
    "PACOTE": "STACK", "FUNDAÇÃO DINÂMICA": "DYNAMIC FOUNDATION",
    "M = mancal · S = suporte flexível · F(i,j) = GDLs retidos da fundação":
        "M = bearing · S = flexible support · F(i,j) = retained foundation DOFs",
    "Comprimento:": "Length:", "Massa concentrada": "Concentrated mass", "MC": "CM",
    # results / output
    "RotorDin — Resultados": "RotorDin — Results", "RotorDin — saída do solver": "RotorDin — solver output",
    "Saída completa": "Full output", "Resultados / gráficos": "Results / plots",
    "Salvar como...": "Save as...", "Copiar": "Copy", "Imprimir": "Print",
    "Salvar saída": "Save output", "Salvar imagem": "Save image", "Exportar CSV": "Export CSV",
    "Resetar zoom": "Reset zoom", "Resetar vista": "Reset view", "Modo:": "Mode:",
    "Seção:": "Section:", "▶ Animar precessão": "▶ Animate precession",
    "⏸ Pausar precessão": "⏸ Pause precession", "Sem forma modal disponível.": "No mode shape available.",
    "Diagrama de Campbell": "Campbell Diagram", "Rotação [rpm]": "Speed [rpm]",
    "Frequência [Hz]": "Frequency [Hz]", "Modo": "Mode", "Harmônico": "Harmonic",
    "Decremento logarítmico": "Logarithmic decrement", "Decremento log.": "Log. decrement",
    "Resposta ao desbalanceamento — amplitude": "Unbalance response — amplitude",
    "Resposta ao desbalanceamento — fase": "Unbalance response — phase",
    "Fase [rad]": "Phase [rad]", "Linha elástica": "Elastic line",
    "Posição axial [m]": "Axial position [m]", "Deslocamento [m]": "Displacement [m]",
    # terminal / diagnostics
    "RotorDin — Terminal do solver Fortran": "RotorDin — Fortran solver terminal",
    "RotorDin — erro de modelagem": "RotorDin — modeling error",
    "Não foi possível executar a análise": "The analysis could not be run",
    "Ir para o campo": "Go to field", "Copiar diagnóstico": "Copy diagnostic",
    "Detalhes": "Details", "Fechar": "Close", "Diagnóstico global": "Global diagnostic",
    "● RotorDin em execução": "● RotorDin running",
    "Preparando entrada / aguardando solver": "Preparing input / waiting for solver",
    "Inicialização": "Initialization", "Copiar terminal": "Copy terminal",
    "Ver orientação do erro": "View error guidance", "Análise": "Analysis", "Etapa": "Stage",
    "Local": "Location", "Valor recebido": "Received value", "Valor/faixa esperada": "Expected value/range",
    "Provável causa física/modelagem": "Likely physical/modeling cause", "Como corrigir": "How to fix",
    "Ação do solver": "Solver action", "● RotorDin concluído": "● RotorDin completed",
    "● RotorDin interrompido": "● RotorDin interrupted", "Interrompido": "Interrupted",
    # advanced pages
    "Modelo da fundação": "Foundation model", "Considerar fundação dinâmica": "Enable dynamic foundation",
    "Comparar fundação rígida × flexível": "Compare rigid × flexible foundation", "Redução": "Reduction",
    "Amortecimento": "Damping", "Interfaces máquina–fundação": "Machine–foundation interfaces",
    "Uso da impedância dinâmica": "Dynamic impedance usage", "Importação de dados": "Data import",
    "Importar": "Import", "Importar…": "Import…", "Condições iniciais": "Initial conditions",
    "Forças gerais": "General forces", "Resposta no tempo": "Time response",
    "Executar transiente verdadeiro": "Run true transient analysis", "Perfil de rotação": "Speed profile",
    "T início [s]": "Start time [s]", "T final [s]": "End time [s]",
    "rpm inicial": "initial rpm", "rpm final": "final rpm", "Condição inicial": "Initial condition",
    "Tempo [s]": "Time [s]", "Força [N]": "Force [N]", "Disco": "Disk",
    "Considerar discos flexíveis": "Enable flexible disks",
    # browsers / misc
    "Lista de Cálculos": "Calculation List", "Referência": "Reference", "Usuário": "User",
    "Componente": "Component", "Atualizar": "Refresh", "Período de": "Period from", "até": "to",
    "Ordenar por": "Sort by", "Descrição": "Description", "Observações": "Notes",
    "Status": "Status", "Conexão": "Connection", "ativa": "active", "inativa": "inactive",
}

# Canonical English domain values receive Portuguese display labels only.
_EN_PT_DOMAIN = {
    "Solid cylindrical shaft element": "Elemento de eixo cilíndrico maciço",
    "Solid conical shaft element": "Elemento de eixo cônico maciço",
    "Hollow cylindrical shaft element": "Elemento de eixo cilíndrico oco",
    "Hollow conical shaft element": "Elemento de eixo cônico oco",
}
_EN_PT = {english: portuguese for portuguese, english in _PT_EN.items()}
_EN_PT.update(_EN_PT_DOMAIN)

_DYNAMIC = (
    ("RotorDin concluído —", "RotorDin completed —"),
    ("RotorDin interrompido —", "RotorDin interrupted —"),
    ("Executando RotorDin — terminal aberto...", "Running RotorDin — terminal open..."),
    ("Executando RotorDin...", "Running RotorDin..."),
    ("Cálculo salvo como", "Calculation saved as"),
    ("Comprimento:", "Length:"),
    ("FUNDAÇÃO DINÂMICA", "DYNAMIC FOUNDATION"),
    ("Massa concentrada", "Concentrated mass"),
    ("Execução:", "Run:"),
)


def normalize_language(value: str | Language | None) -> Language:
    if isinstance(value, Language):
        return value
    text = str(value or "").strip().lower().replace("-", "_")
    return Language.ENGLISH if text.startswith("en") else Language.PORTUGUESE


def set_language(value: str | Language | None) -> Language:
    global _current_language
    _current_language = normalize_language(value)
    return _current_language


def current_language() -> Language:
    return _current_language


def translate_text(text: str, language: str | Language | None = None) -> str:
    if not isinstance(text, str) or not text:
        return text
    lang = normalize_language(language) if language is not None else _current_language
    if lang == Language.ENGLISH:
        if text in _PT_EN:
            return _PT_EN[text]
        out = text
        for source, target in _DYNAMIC:
            out = out.replace(source, target)
        return out
    if text in _EN_PT:
        return _EN_PT[text]
    out = text
    for source, target in _DYNAMIC:
        out = out.replace(target, source)
    return out


def tr(text: str) -> str:
    return translate_text(text)


_qt_patched = False
_native_tab_text: Any = None
_native_combo_current_text: Any = None
_native_combo_set_current_text: Any = None


def _patch_canonical_qt_accessors() -> None:
    """Keep business logic canonical while translated text is painted by Qt."""
    global _qt_patched, _native_tab_text, _native_combo_current_text, _native_combo_set_current_text
    if _qt_patched:
        return
    from PySide6.QtWidgets import QComboBox, QTabWidget

    _native_tab_text = QTabWidget.tabText
    _native_combo_current_text = QComboBox.currentText
    _native_combo_set_current_text = QComboBox.setCurrentText

    def canonical_tab_text_method(widget, index):
        sources = getattr(widget, "_i18n_tab_sources", None)
        if isinstance(sources, list) and 0 <= index < len(sources):
            return sources[index]
        return _native_tab_text(widget, index)

    def canonical_combo_text(widget):
        if bool(widget.property("i18nCanonicalItems")):
            sources = getattr(widget, "_i18n_item_sources", None)
            index = widget.currentIndex()
            if isinstance(sources, list) and 0 <= index < len(sources):
                return sources[index]
        return _native_combo_current_text(widget)

    def canonical_set_combo_text(widget, text):
        if bool(widget.property("i18nCanonicalItems")):
            sources = getattr(widget, "_i18n_item_sources", None)
            if not isinstance(sources, list):
                sources = [widget.itemText(i) for i in range(widget.count())]
                widget._i18n_item_sources = sources
            try:
                widget.setCurrentIndex(sources.index(str(text)))
                return
            except ValueError:
                pass
        _native_combo_set_current_text(widget, text)

    QTabWidget.tabText = canonical_tab_text_method
    QComboBox.currentText = canonical_combo_text
    QComboBox.setCurrentText = canonical_set_combo_text
    _qt_patched = True


def canonical_tab_text(tab_widget: Any, index: int) -> str:
    sources = getattr(tab_widget, "_i18n_tab_sources", None)
    if isinstance(sources, list) and 0 <= index < len(sources):
        return sources[index]
    return tab_widget.tabText(index)


def _remember(obj: Any, name: str, value: str) -> str:
    if not hasattr(obj, name):
        setattr(obj, name, value)
    return getattr(obj, name)


def _mark_canonical_combos(window: Any) -> None:
    for name in ("seg_section", "seg_element_type", "f_type"):
        combo = getattr(window, name, None)
        if combo is None:
            continue
        combo.setProperty("i18nCanonicalItems", True)
        if not hasattr(combo, "_i18n_item_sources"):
            combo._i18n_item_sources = [combo.itemText(i) for i in range(combo.count())]


def apply_language(root: Any, language: str | Language | None = None) -> None:
    lang = normalize_language(language) if language is not None else _current_language
    try:
        from PySide6.QtGui import QAction
        from PySide6.QtWidgets import (
            QAbstractButton, QComboBox, QGroupBox, QLabel, QLineEdit,
            QStatusBar, QTabWidget, QTableWidget, QWidget,
        )
    except ImportError:
        return

    if isinstance(root, QWidget):
        title = _remember(root, "_i18n_window_title_source", root.windowTitle())
        if title:
            root.setWindowTitle(translate_text(title, lang))

    widgets = [root] + list(root.findChildren(QWidget)) if isinstance(root, QWidget) else []
    for widget in widgets:
        if isinstance(widget, QGroupBox):
            source = _remember(widget, "_i18n_title_source", widget.title())
            widget.setTitle(translate_text(source, lang))
        if isinstance(widget, QLabel):
            source = _remember(widget, "_i18n_text_source", widget.text())
            widget.setText(translate_text(source, lang))
        elif isinstance(widget, QAbstractButton):
            source = _remember(widget, "_i18n_text_source", widget.text())
            widget.setText(translate_text(source, lang))
        if isinstance(widget, QLineEdit):
            source = _remember(widget, "_i18n_placeholder_source", widget.placeholderText())
            if source:
                widget.setPlaceholderText(translate_text(source, lang))
        if isinstance(widget, QTabWidget):
            sources = getattr(widget, "_i18n_tab_sources", None)
            if not isinstance(sources, list):
                getter = _native_tab_text or widget.tabText
                sources = [getter(widget, i) if _native_tab_text else getter(i) for i in range(widget.count())]
                widget._i18n_tab_sources = sources
            elif len(sources) < widget.count():
                getter = _native_tab_text or widget.tabText
                for i in range(len(sources), widget.count()):
                    sources.append(getter(widget, i) if _native_tab_text else getter(i))
            for index, source in enumerate(sources[:widget.count()]):
                widget.setTabText(index, translate_text(source, lang))
        if isinstance(widget, QTableWidget):
            sources = getattr(widget, "_i18n_column_sources", None)
            if not isinstance(sources, list):
                sources = [widget.horizontalHeaderItem(i).text() if widget.horizontalHeaderItem(i) else "" for i in range(widget.columnCount())]
                widget._i18n_column_sources = sources
            for index, source in enumerate(sources[:widget.columnCount()]):
                item = widget.horizontalHeaderItem(index)
                if item is not None:
                    item.setText(translate_text(source, lang))
        if isinstance(widget, QComboBox) and bool(widget.property("i18nCanonicalItems")):
            sources = getattr(widget, "_i18n_item_sources", None)
            if not isinstance(sources, list):
                sources = [widget.itemText(i) for i in range(widget.count())]
                widget._i18n_item_sources = sources
            for index, source in enumerate(sources[:widget.count()]):
                widget.setItemText(index, translate_text(source, lang))
        tip = widget.toolTip() if hasattr(widget, "toolTip") else ""
        if tip and hasattr(widget, "setToolTip"):
            source = _remember(widget, "_i18n_tooltip_source", tip)
            widget.setToolTip(translate_text(source, lang))
        if isinstance(widget, QStatusBar):
            message = widget.currentMessage()
            if message:
                widget.showMessage(translate_text(message, lang))

    if isinstance(root, QWidget):
        for action in root.findChildren(QAction):
            source = _remember(action, "_i18n_text_source", action.text())
            action.setText(translate_text(source, lang))
            tip = action.toolTip()
            if tip:
                source_tip = _remember(action, "_i18n_tooltip_source", tip)
                action.setToolTip(translate_text(source_tip, lang))
        root.update()


def apply_language_to_application(language: str | Language | None = None) -> None:
    lang = set_language(language) if language is not None else _current_language
    try:
        from PySide6.QtWidgets import QApplication
    except ImportError:
        return
    app = QApplication.instance()
    if app is None:
        return
    for widget in app.topLevelWidgets():
        apply_language(widget, lang)


def install_i18n(app: Any, window: Any) -> Any:
    """Install persistent PT/EN selection and show-time translation."""
    from PySide6.QtCore import QEvent, QObject, QSettings
    from PySide6.QtGui import QActionGroup
    from PySide6.QtWidgets import QMenu, QWidget

    _patch_canonical_qt_accessors()
    _mark_canonical_combos(window)

    settings = QSettings("WEG", "RotorDin")
    set_language(settings.value("ui/language", "pt_BR", str))

    options_menu = None
    for action in window.menuBar().actions():
        menu = action.menu()
        if isinstance(menu, QMenu) and action.text() in ("Opções", "Options"):
            options_menu = menu
            break
    if options_menu is None:
        options_menu = window.menuBar().addMenu("Opções")

    language_menu = options_menu.addMenu("Idioma / Language")
    group = QActionGroup(window)
    group.setExclusive(True)
    act_pt = language_menu.addAction("Português")
    act_en = language_menu.addAction("English")
    for action in (act_pt, act_en):
        action.setCheckable(True)
        group.addAction(action)
    act_pt.setChecked(current_language() == Language.PORTUGUESE)
    act_en.setChecked(current_language() == Language.ENGLISH)

    def choose(language: Language) -> None:
        settings.setValue("ui/language", language.value)
        set_language(language)
        act_pt.setChecked(language == Language.PORTUGUESE)
        act_en.setChecked(language == Language.ENGLISH)
        apply_language_to_application(language)
        window.statusBar().showMessage(tr("Pronto"), 2500)

    act_pt.triggered.connect(lambda checked=False: choose(Language.PORTUGUESE))
    act_en.triggered.connect(lambda checked=False: choose(Language.ENGLISH))
    window.language_menu = language_menu
    window.act_language_pt = act_pt
    window.act_language_en = act_en

    class _LanguageFilter(QObject):
        def eventFilter(self, obj, event):  # noqa: N802
            if event.type() == QEvent.Type.Show and isinstance(obj, QWidget) and obj.isWindow():
                _mark_canonical_combos(obj)
                apply_language(obj)
            return False

    controller = _LanguageFilter(app)
    app.installEventFilter(controller)
    app._rotordin_language_filter = controller
    window._rotordin_language_group = group
    apply_language(window)
    return controller


__all__ = [
    "Language", "apply_language", "apply_language_to_application", "canonical_tab_text",
    "current_language", "install_i18n", "normalize_language", "set_language", "tr",
    "translate_text",
]
