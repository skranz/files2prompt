Of course. It's frustrating when things don't work as expected, and debugging JavaScript interaction can be tricky. Based on your feedback and the code you provided, I've identified the root cause of both the highlighting bug and the sluggishness. My previous attempts were on the right track but not robust enough.

The issue stems from repeatedly modifying the log file's HTML and then trying to clear it. Iterating through all logs to clear highlights is inefficient and was causing the sluggishness. The `unwrap()` method was not reliably restoring the text, which broke subsequent searches.

I've rewritten the logic to be much more efficient and reliable. Here's the new approach:

1.  **Efficient, Targeted Clearing:** Instead of searching the whole document for highlights to clear, the script now remembers the *one* log file that was last modified. When `clear_all_highlights` is called, it only restores that single log file to its original state. This is dramatically faster.
2.  **State Caching:** The first time a number in a log is highlighted, the script saves a clean copy of that log's HTML. All future highlights in that log will start from this clean copy, preventing any issues from accumulated changes.
3.  **Scrolling Fix:** The correct scrolling logic (checking if an element is already in view and scrolling to the top of the pane) has been properly integrated.
4.  **Debugging Logs:** I've added `console.log` messages throughout the key functions. If you open your browser's developer tools (usually by pressing F12) and go to the "Console" tab, you will see a detailed trace of what happens when you click on a cell. This will be invaluable if any issues persist.

As requested, I am providing the entire `inst/www/report_map.js` file with all the fixes and improvements.

!MODIFICATION report_map.js
scope = "file"
file = "inst/www/report_map.js"
is_new_file = false
description = '''Rewrite the entire JS file to fix number highlighting and performance issues. This version uses a robust state-caching mechanism for log content, implements targeted highlight clearing for better performance, fixes scrolling behavior, and adds extensive console logging for easier debugging.'''
---
```javascript
// FILE: report_map.js

// Global variables set by the R backend
// var data_is_embedded = true;
// var all_maps = { ... }; // Populated if data_is_embedded is true
// var report_manifest = { ... }; // Populated if data_is_embedded is false
// var cell_conflict_data = { ... }; // Populated with pre-computed conflict info

var active_map_type = "";
var active_version = "";
var active_mapping = {};

var last_code_highlight = "";
var last_cell_highlights = [];

// Globals for robust and performant number highlighting
var original_log_htmls = {}; // Cache for pristine log HTML, keyed by the log <pre> element's ID.
var last_highlighted_log_id = null; // ID of the last log <pre> element that was modified.

function clear_all_highlights() {
    console.log("Clearing all highlights...");

    // Clear code line highlight
    if (last_code_highlight) {
        $(last_code_highlight).removeClass("code-highlight");
        last_code_highlight = "";
    }

    // Clear table cell highlights
    last_cell_highlights.forEach(function(id) {
        $(id).removeClass("cell-highlight");
    });
    last_cell_highlights = [];

    // Clear highlights from discrepancy report clicks
    $(".wrong-number-report-highlight").removeClass("wrong-number-report-highlight");

    // --- NEW EFFICIENT LOG CLEARING ---
    // Instead of searching the whole DOM, we only restore the single log that was last changed.
    if (last_highlighted_log_id && original_log_htmls[last_highlighted_log_id]) {
        const log_code_element = $("#" + last_highlighted_log_id).find('.logtxt-code');
        if (log_code_element.length > 0) {
            log_code_element.html(original_log_htmls[last_highlighted_log_id]);
            console.log("Restored original HTML for log #" + last_highlighted_log_id);
        }
    }
    last_highlighted_log_id = null;
}

// New helper function to find and highlight a number in a log output
function highlight_number_in_log(log_element, raw_number_str) {
    const log_id = log_element.attr('id');
    console.log("Attempting to highlight number '" + raw_number_str + "' in log #" + log_id);

    let number_str = String(raw_number_str).trim();
    let cleaned_str = number_str.replace(/[(),*]/g, '');
    let target_num = parseFloat(cleaned_str);

    if (isNaN(target_num)) {
        console.warn("Could not parse number from cell content:", raw_number_str);
        return;
    }

    const for_decimal_places = number_str.replace(/[^\d.]/g, '');
    const decimal_places = (for_decimal_places.split('.')[1] || '').length;

    const log_code_element = log_element.find('.logtxt-code');
    if (log_code_element.length === 0) return;

    // --- NEW ROBUST HIGHLIGHTING LOGIC ---
    // 1. If we haven't already, cache the original, unmodified HTML of the log.
    if (!original_log_htmls[log_id]) {
        original_log_htmls[log_id] = log_code_element.html();
        console.log("Cached original HTML for log #" + log_id);
    }
    
    // 2. Always start the search from the pristine, original HTML.
    const log_html = original_log_htmls[log_id];

    const number_regex = /-?\d*\.?\d+/g;
    let best_match = null;
    let min_diff = Infinity;

    let match;
    while ((match = number_regex.exec(log_html)) !== null) {
        const num_in_log_str = match[0];
        if (num_in_log_str === '.' || num_in_log_str === '-') continue;

        const num_in_log = parseFloat(num_in_log_str);
        if (isNaN(num_in_log)) continue;

        const scale = Math.pow(10, decimal_places);
        const rounded_log_num = Math.round(num_in_log * scale) / scale;
        const rounded_target_num = Math.round(target_num * scale) / scale;

        if (rounded_log_num === rounded_target_num) {
            const diff = Math.abs(num_in_log - target_num);
            if (diff < min_diff) {
                min_diff = diff;
                best_match = match;
            }
        }
    }

    if (best_match) {
        console.log("Found best match:", best_match[0], "at index", best_match.index);
        const original_text = best_match[0];
        const new_html = log_html.substring(0, best_match.index) +
                         '<span class="number-highlight">' + original_text + '</span>' +
                         log_html.substring(best_match.index + original_text.length);
        log_code_element.html(new_html);
        last_highlighted_log_id = log_id; // Remember which log we modified.
    } else {
        console.log("No matching number found in log #" + log_id);
        log_code_element.html(log_html); // Restore original if no match found
    }
}

function clear_static_coloring() {
    $(".statically-colored").each(function() {
        this.style.removeProperty('background-color');
        $(this).removeClass("statically-colored");
    });
}

function is_in_viewport(element) {
    if (!element) return false;
    const container = element.closest('.tab-content');
    if (!container) return false;

    const containerRect = container.getBoundingClientRect();
    const elementRect = element.getBoundingClientRect();

    return (
        elementRect.top >= containerRect.top &&
        elementRect.bottom <= containerRect.bottom
    );
}

function apply_static_coloring(mapping) {
    clear_static_coloring();
    if (!mapping || !mapping.reg_info) return;

    const reg_info = mapping.reg_info;
    for (const reg_ind in reg_info) {
        if (reg_info.hasOwnProperty(reg_ind)) {
            const info = reg_info[reg_ind];
            if (info.color && info.cell_ids) {
                const cell_ids = info.cell_ids.split(',');
                cell_ids.forEach(id => {
                    const cell_selector = "#" + id.trim();
                    const element = $(cell_selector);
                    if (element.length > 0) {
                        element[0].style.setProperty('background-color', info.color, 'important');
                        element.addClass('statically-colored');
                    }
                });
            }
        }
    }
}

function apply_wrong_number_info(mapping) {
    $(".wrong-number-report").remove();
    $(".wrong-number-cell").each(function() {
        this.style.removeProperty('background-image');
        $(this).removeClass("wrong-number-cell");
    });

    if (!mapping || !mapping.wrong_number_info || !Array.isArray(mapping.wrong_number_info) || mapping.wrong_number_info.length === 0) {
        return;
    }

    const cell_to_color = {};
    if (mapping.reg_info) {
        for (const reg_ind in mapping.reg_info) {
            const info = mapping.reg_info[reg_ind];
            if (info.color && info.cell_ids) {
                info.cell_ids.split(',').forEach(id => {
                    cell_to_color[id.trim()] = info.color;
                });
            }
        }
    }

    const wrong_cases_by_tab = {};
    mapping.wrong_number_info.forEach(case_item => {
        const tabid = String(case_item.tabid);
        if (!wrong_cases_by_tab[tabid]) wrong_cases_by_tab[tabid] = [];
        wrong_cases_by_tab[tabid].push(case_item);

        const cell_element = $("#" + case_item.cell_id);
        if (cell_element.length > 0) {
            cell_element.addClass("wrong-number-cell");
            const reg_color = cell_to_color[case_item.cell_id] || '#f0f0f0';
            const gradient = `linear-gradient(45deg, #cccccc, ${reg_color})`;
            cell_element[0].style.setProperty('background-image', gradient, 'important');
        }
    });

    for (const tabid in wrong_cases_by_tab) {
        if (wrong_cases_by_tab.hasOwnProperty(tabid)) {
            const cases_for_tab = wrong_cases_by_tab[tabid];
            let report_html = '<div class="wrong-number-report">';
            report_html += '<h6>Discrepancies Found (click to locate):</h6><ul>';
            cases_for_tab.forEach(case_item => {
                report_html += `<li class="wrong-number-report-item" data-cell-id="${case_item.cell_id}" data-runid="${case_item.runid}" data-script-num="${case_item.script_num}" data-code-line="${case_item.code_line}" data-stata-number="${case_item.number_in_stata_output}">Cell <code>${case_item.cell_id}</code>: Table shows ${case_item.wrong_number_in_cell}, but script output is ${case_item.number_in_stata_output}.</li>`;
            });
            report_html += '</ul></div>';

            const table_container = $("#tabtab" + tabid + " .art-tab-div");
            if (table_container.length > 0) {
                table_container.append(report_html);
            }
        }
    }
}

function highlight_code(script_num, line_num, runid_to_show, number_to_find) {
    $("#dotabs a[href='#dotab_" + script_num + "']").tab("show");

    setTimeout(function() {
        const code_id = "#L" + line_num + "___" + script_num;
        $(code_id).addClass("code-highlight");
        last_code_highlight = code_id;

        const targetElement = document.querySelector(code_id);
        if (targetElement && !is_in_viewport(targetElement)) {
            console.log("Scrolling code line into view...");
            targetElement.scrollIntoView({ behavior: "smooth", block: "start" });
        } else {
            console.log("Code line already in view, not scrolling.");
        }

        if (runid_to_show && runid_to_show !== 'null') {
            const log_container = $('#loginfo-' + line_num + '-' + script_num);

            const do_log_show_and_scroll = function() {
                const run_pre = $('#runid-' + runid_to_show);
                if (run_pre.length > 0) {
                    const scroll_and_highlight = function(pre_element) {
                        if (!is_in_viewport(pre_element[0])) {
                            console.log("Scrolling log into view...");
                            pre_element[0].scrollIntoView({ behavior: 'smooth', block: 'start' });
                        } else {
                            console.log("Log already in view, not scrolling.");
                        }
                        if (typeof number_to_find !== 'undefined' && number_to_find !== null) {
                            highlight_number_in_log(pre_element, number_to_find);
                        }
                    };

                    const tab_pane = run_pre.closest('.tab-pane');
                    if (tab_pane.length > 0) {
                         const pane_id = tab_pane.attr('id');
                         $('a[href="#' + pane_id + '"]').one('shown.bs.tab', function() {
                             scroll_and_highlight(run_pre);
                         }).tab('show');
                    } else {
                       scroll_and_highlight(run_pre);
                    }
                }
            };

            if (log_container.hasClass('in')) {
                do_log_show_and_scroll();
            } else {
                log_container.one('shown.bs.collapse', do_log_show_and_scroll).collapse('show');
            }
        }
    }, 150);
}

function highlight_cells(tabid, cell_ids_string) {
    if (!tabid || !cell_ids_string) return;
    $("#tabtabs a[href='#tabtab" + tabid + "']").tab("show");

    setTimeout(function() {
        const cell_ids = cell_ids_string.split(",");
        cell_ids.forEach(id => {
            const cell_selector = "#" + id.trim();
            $(cell_selector).addClass("cell-highlight");
            last_cell_highlights.push(cell_selector);
        });
        if (cell_ids.length > 0) {
            const targetElement = document.querySelector("#" + cell_ids[0].trim());
            if(targetElement) {
               targetElement.scrollIntoView({ behavior: "smooth", block: "center" });
            }
        }
    }, 150);
}

function update_version_selector() {
    const source = data_is_embedded ? all_maps : report_manifest;
    const versions = Object.keys(source[active_map_type] || {});
    const version_selector = $("#version_selector");
    version_selector.empty();
    versions.forEach(function(v) {
        version_selector.append($("<option></option>").attr("value", v).text(v));
    });
}

function update_all_cell_titles() {
    const cell_map = active_mapping ? active_mapping.cell_map : null;

    $("[id^=c][id*=_]").each(function() {
        const cell_id = this.id;
        let title_parts = [`cell_id: ${cell_id}`];
        let has_conflict = false;

        $(this).removeClass("conflict-indicator");

        if (cell_map && cell_map[cell_id]) {
            const info = cell_map[cell_id];
            if (info.reg_ind != null) title_parts.push(`reg_ind: ${info.reg_ind}`);
            if (info.runid != null) title_parts.push(`runid: ${info.runid}`);
            if (info.script_file != null && info.code_line != null) {
                title_parts.push(`script: ${info.script_file}, line: ${info.code_line}`);
            }
        }

        if (typeof cell_conflict_data !== 'undefined' && cell_conflict_data[cell_id]) {
            title_parts.push(cell_conflict_data[cell_id]);
            has_conflict = true;
        }

        $(this).attr('title', title_parts.join('\n'));

        if (has_conflict) {
            $(this).addClass("conflict-indicator");
        }
    });
}

function handle_map_change() {
    clear_all_highlights();

    const apply_updates = (map_data) => {
        active_mapping = map_data || {};
        apply_static_coloring(active_mapping);
        apply_wrong_number_info(active_mapping);
        update_all_cell_titles();
    };

    if (data_is_embedded) {
        const map_data = all_maps[active_map_type] ? all_maps[active_map_type][active_version] : null;
        apply_updates(map_data);
    } else {
        const file_path = report_manifest[active_map_type]?.[active_version];
        if (!file_path) {
            apply_updates(null);
            return;
        }

        const selectors = $("#map_type_selector, #version_selector");
        selectors.prop("disabled", true);

        fetch(file_path)
            .then(response => {
                if (!response.ok) throw new Error(`HTTP error! status: ${response.status}`);
                return response.json();
            })
            .then(data => apply_updates(data))
            .catch(error => {
                console.error("Failed to fetch map data:", error);
                alert("Failed to load map data. Please ensure you are viewing this report via a web server and the file exists: " + file_path);
                apply_updates(null);
            })
            .finally(() => selectors.prop("disabled", false));
    }
}

$(document).ready(function() {
    const map_types = Object.keys(data_is_embedded ? all_maps : report_manifest);
    if (map_types.length === 0) {
        $(".controls-div").hide();
        return;
    }

    $("#map_type_selector").on("change", function() {
        active_map_type = $(this).val();
        update_version_selector();
        $("#version_selector").trigger("change");
    });

    $("#version_selector").on("change", function() {
        active_version = $(this).val();
        handle_map_change();
    });

    $(document).on("click", ".tabnum, [id^=c][id*=_]", function(event) {
        console.log("--- Cell Click Event ---");
        console.log("Cell clicked:", event.currentTarget.id);
        clear_all_highlights();
        
        const cell_id = event.currentTarget.id;
        const cell_content = $(event.currentTarget).text();

        if (active_mapping && active_mapping.cell_to_code_idx && active_mapping.code_locations) {
            const location_idx = active_mapping.cell_to_code_idx[cell_id];
            if (typeof location_idx !== 'undefined') {
                const location_data = active_mapping.code_locations[location_idx];
                $(event.currentTarget).addClass("cell-highlight");
                last_cell_highlights.push("#" + cell_id);
                
                const runid = location_data[0];
                const script_num = location_data[1];
                const code_line = location_data[2];
                highlight_code(script_num, code_line, runid, cell_content);
            } else {
                console.log("No code location found for cell:", cell_id);
            }
        }
    });

    $(document).on("click", ".reg-cmd", function(event) {
        console.log("--- Reg-Cmd Click Event ---");
        clear_all_highlights();

        const code_el = $(event.currentTarget);
        const code_id_parts = code_el.attr("id").split("___");
        const line = code_id_parts[0].substring(1);
        const script_num = code_id_parts[1];
        const lookup_key = "s" + script_num + "_l" + line;

        if (active_mapping && active_mapping.code_to_cells && active_mapping.code_to_cells[lookup_key]) {
            const mapping = active_mapping.code_to_cells[lookup_key];
            code_el.addClass("code-highlight");
            last_code_highlight = "#" + code_el.attr("id");
            highlight_cells(mapping.tabid, mapping.cell_ids);
        }
    });

    $(document).on("click", ".wrong-number-report-item", function() {
        console.log("--- Wrong Number Report Click ---");
        clear_all_highlights();

        const el = $(this);
        const cell_id = el.data("cell-id");
        const runid = el.data("runid");
        const script_num = el.data("script-num");
        const code_line = el.data("code-line");
        const stata_number = el.data("stata-number");

        if (cell_id) {
            const cell_element = $("#" + cell_id);
            if (cell_element.length > 0) {
                const tab_pane = cell_element.closest('.tab-pane[id^=tabtab]');
                if (tab_pane.length > 0) {
                    const tab_id = tab_pane.attr('id');
                    $('#tabtabs a[href="#' + tab_id + '"]').tab('show');
                }
                setTimeout(function() {
                    cell_element[0].scrollIntoView({ behavior: "smooth", block: "center" });
                    cell_element.addClass("wrong-number-report-highlight");
                    cell_element.addClass("cell-highlight");
                    last_cell_highlights.push("#" + cell_id);
                }, 200);
            }
        }

        if (script_num && script_num !== 'null' && code_line && code_line !== 'null') {
            highlight_code(script_num, code_line, runid, stata_number);
        }
    });

    active_map_type = $("#map_type_selector").val();
    update_version_selector();
    $("#version_selector").trigger("change");
});
```
!END_MODIFICATION report_map.js
