/**
 * The protocol between the patient profile's R side and its client.
 *
 * Dev tooling only: type-checked by tsconfig.json / `tsc`, never shipped.
 * The R side of the same table is the roxygen on pp_send() (R/pp-messages.R)
 * for messages, and the input observers in patient-profile-block.R for
 * inputs. jsonlite gives R no types, so this file is where the shapes are
 * checked; tests/js pins what each handler does with them.
 */

/* --- What R sends: session$sendCustomMessage(session$ns(channel), payload) --- */

/** `sync_selected`: the panel ids on the profile, in order. Always an
 *  array: pp_send() wraps it, so one panel is `["x"]`, none is `[]`. */
type PpSyncSelected = string[];

/** `sync_subject`: the picked patient, `""` for none. */
interface PpSyncSubject { id: string }

/** `sync_params`: `viz@@PARAMCD` keys of the parameters on screen. */
type PpSyncParams = string[];

/** `sync_band`: which panel the cohort strip draws, `""` for none. */
interface PpSyncBand { viz_id: string }

/** `subject_picker`: how many patients the cohort holds. */
interface PpSubjectPicker { count: number }

/** `dl_menu_state`: what the download menu may offer. */
interface PpDlMenuState {
  /** exactly one patient is picked */
  single: boolean;
  /** that patient's id, `""` otherwise */
  picked: string;
  /** patients in the incoming tables */
  n: number;
}

/** `slot`: bring one panel to the current patient without rebuilding it
 *  (R: pp_slot_update()). */
interface PpSlotUpdate {
  viz_id: string;
  /** the whole panel header as HTML, root included (pp_slot_header_ui() rendered) */
  header: string;
  /** the echarts option, serialised by htmlwidgets' encoder */
  opts_json: string;
  /** paths of the JS() strings in it, htmlwidgets style: "series.0.renderItem" */
  evals: string[];
  /** the widget's height in px, or null */
  height: number | null;
}

/** One term the picker can offer, with a count. In the list that rides the
 *  header `n` is this patient's RECORDS; in the cohort vocabulary below it is
 *  the number of PATIENTS who have it. */
interface PpFindOption { value: string; n: number }

/** The terms of one coding level. */
interface PpFindGroup {
  /** the column, which is what a pick off this group is matched against */
  col: string;
  /** its display name ("Body system") */
  label: string;
  options: PpFindOption[];
  /** how many the cap left out, or 0 */
  truncated: number;
}

/** `find_vocab`: the COHORT's terms for one panel, in reply to the input of
 *  the same name. `unchanged` means the client's token is still current and
 *  no list was sent. */
interface PpFindVocab {
  viz_id: string;
  token: string;
  groups?: PpFindGroup[];
  unchanged?: boolean;
}

/* --- What the client sends: Shiny.setInputValue(ns(name), value) --- */

/** `pick_param`: a parameter row, for a multi-parameter panel. */
interface PpPickParam { viz_id: string; paramcd: string }

/** One filter on a panel's find control. `col` is the coding level the term
 *  came from and the value is matched against that column exactly; the one
 *  exception is `"*"`, the free-text pick, which is a substring across every
 *  column the viz declares (R: pp_find_picks()). */
interface PpFindPick { col: string; value: string }

/** `find_vocab`: asking for the cohort's terms. `have` is the token of the
 *  list this client already holds, `""` for none. */
interface PpFindVocabRequest { viz_id: string; have: string }

/** `viz_ctrl`: one control in a panel header changed. */
interface PpVizCtrl {
  viz_id: string;
  param: string;
  /** a pill or radio choice, a toggle, the active chips, or the find picks */
  value: string | boolean | string[] | PpFindPick[];
}

/** Every input by name suffix, with the value it carries. */
interface PpInputs {
  pick_subject: string;
  cohort_sort: string;
  toggle_viz: string;
  pick_param: PpPickParam;
  reorder_viz: string[];
  viz_ctrl: PpVizCtrl;
  find_vocab: PpFindVocabRequest;
  timeline_mode: 'rday' | 'date';
  show_prestudy: boolean;
  smooth_mode: 'off' | 'auto';
}

/* --- The mount --- */

/** What R hands PatientProfile.mount() (R/patient-profile-block.R). */
interface PpConfig {
  /** the module's namespace; every id is `id + '-' + name` */
  id: string;
  /** the drag-handle glyph the On rows reuse (pp_grip_glyph()) */
  grip: string;
  /** height of a spans band (pp_cohort_band_h_spans) */
  bandH: number;
}

interface PpContext {
  cfg: PpConfig;
  ns: (name: string) => string;
}

type PpPart = (ctx: PpContext) => void;

interface PatientProfileNamespace {
  part(fn: PpPart): void;
  mount(cfg: PpConfig): PpContext;
}

/* --- Globals the files reach for --- */

interface ShinyStatic {
  setInputValue(name: string, value: unknown, opts?: { priority?: 'event' | 'immediate' | 'deferred' }): void;
  addCustomMessageHandler(name: string, handler: (msg: any) => void): void;
  bindAll?(scope: Element): void;
  unbindAll?(scope: Element): void;
}

interface EchartsInstance {
  resize(): void;
  setOption(opts: object, opts2?: { notMerge?: boolean }): void;
}

interface EchartsStatic {
  getInstanceByDom(el: Element): EchartsInstance | null;
}

declare var Shiny: ShinyStatic;
declare var echarts: EchartsStatic | undefined;
declare var PatientProfile: PatientProfileNamespace;
declare var $: JQueryStatic;
declare var jQuery: JQueryStatic;

interface Window {
  Shiny: ShinyStatic;
  echarts?: EchartsStatic;
  PatientProfile: PatientProfileNamespace;
}

/** The slice of jQuery these files use; Shiny ships jQuery 3. */
interface JQueryStatic {
  (selector: string | Element | Document | Window | (() => void) | JQuery): JQuery;
  Event: new (type: string, props?: object) => JQueryEvent;
}
interface JQueryEvent {
  name?: string;
  target: Element;
  originalEvent?: Event;
}
interface JQuery {
  length: number;
  [index: number]: HTMLElement;
  on(events: string, handler: (this: HTMLElement, e: any) => void): JQuery;
  on(events: string, selector: string, handler: (this: HTMLElement, e: any) => void): JQuery;
  trigger(event: string | object): JQuery;
  find(selector: string): JQuery;
  filter(selector: string): JQuery;
  first(): JQuery;
  closest(selector: string): JQuery;
  siblings(selector?: string): JQuery;
  each(fn: (this: HTMLElement, index: number, el: HTMLElement) => void): JQuery;
  attr(name: string): string | undefined;
  attr(name: string, value: string | number | null): JQuery;
  removeAttr(name: string): JQuery;
  data(name: string): any;
  val(): string | number | string[] | undefined;
  val(value: string): JQuery;
  text(): string;
  text(value: string | number): JQuery;
  html(): string;
  html(value: string): JQuery;
  addClass(cls: string): JQuery;
  removeClass(cls: string): JQuery;
  toggleClass(cls: string, state?: boolean): JQuery;
  hasClass(cls: string): boolean;
  is(target: string | Element): boolean;
  has(target: Element | string): JQuery;
  focus(): JQuery;
  empty(): JQuery;
}
