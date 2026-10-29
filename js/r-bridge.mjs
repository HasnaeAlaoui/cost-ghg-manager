// Transport only: fixed R entry points, scalar inputs, plain JSON outputs.
// Never retain RObject proxies or manually manage shelters here.
const calls = Object.freeze({
  simulate_activity_response_json: 'simulate_activity_response_json(arg0, arg1, arg2)',
  get_order_comparison_json: 'get_order_comparison_json(arg0)',
  get_comparison_report_json: 'get_comparison_report_json(arg0, arg1)',
  get_comparison_nodes_json: 'get_comparison_nodes_json(arg0)',
  get_node_variance_json: 'get_node_variance_json(arg0)',
  preview_csv_json: 'preview_csv_json(arg0, arg1, arg2)',
  import_csv_json: 'import_csv_json(arg0, arg1, arg2)',
  export_csv_json: 'export_csv_json(arg0)',
  get_dashboard_json: 'get_dashboard_json(arg0)',
  get_report_json: 'get_report_json(arg0)',
  calculate_variance_json: 'calculate_variance_json(arg0)',
  get_accounting_view_json: 'accounting_view_json(arg0)',
  calculate_standard_view_json: 'calculate_accounting_json(arg0, FALSE)',
  calculate_actual_json: 'calculate_accounting_json(arg0, TRUE)',
  get_standard_context_json: 'get_standard_context_json()',
  calculate_standard_json: 'calculate_standard_json(arg0)',
  get_activity_response_json: 'get_activity_response_json(arg0, arg1)',
  execute_production_order: 'execute_production_order(arg0)',
  update_activity_response: 'update_activity_response(arg0, arg1, arg2)',
  get_production_order_json: 'get_production_order_json(arg0)',
  update_production_order: 'update_production_order(arg0, arg1, arg2, arg3, arg4)',
  delete_production_order: 'delete_production_order(arg0)',
  export_visual_project_json: 'export_visual_project_json(arg0, arg1)',
  get_production_json: 'get_production_json()',
  get_order_production_json: 'get_production_json(arg0)',
  create_production_order: 'create_production_order(arg0, arg1, arg2, arg3)',
  generate_pro_node_tree: 'generate_pro_node_tree(arg0)', 
  get_definition_graph_json: 'get_definition_graph_json(arg0, arg1, arg2)',
  create_scoped_bom: 'create_bom(arg0, arg1, arg2, arg3, arg4)',
  update_scoped_bom: 'update_bom(arg0, arg1, arg2, arg3, arg4, arg5)',
  create_scoped_route: 'create_route(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)',
  update_scoped_route: 'update_route(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)',
  get_structure_json: 'get_structure_json(arg0, arg1, arg2)',
  get_structure_row_json: 'get_structure_row_json(arg0, arg1)',
  get_bom_graph_json: 'get_bom_graph_json()',
  create_bom: 'create_bom(arg0, arg1, arg2, arg3)',
  update_bom: 'update_bom(arg0, arg1, arg2, arg3, arg4)',
  delete_bom: 'delete_bom(arg0)',
  create_route: 'create_route(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)',
  update_route: 'update_route(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)',
  delete_route: 'delete_route(arg0)',
  get_resource_fields_json: 'get_resource_fields_json()',
  create_equipment: 'create_equipment(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)',
  update_equipment: 'update_equipment(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)',
  delete_equipment: 'delete_equipment(arg0)',
  get_equipment_json: 'get_equipment_json(arg0)',
  get_equipments_json: 'get_equipment_json()',
  create_energy: 'create_energy(arg0, arg1, arg2, arg3)',
  update_energy: 'update_energy(arg0, arg1, arg2, arg3, arg4)',
  delete_energy: 'delete_energy(arg0)',
  get_energy_json: 'get_energy_json(arg0)',
  get_energies_json: 'get_energy_json()',
  get_activity_categories_json: 'get_activity_categories_json()',
  export_named_project_json: 'export_project_json(arg0)',
  export_project_json: 'export_project_json()',
  restore_project: 'restore_project(arg0)',
  reset_project: 'reset_project()',
  get_project_status_json: 'get_project_status_json()',
  create_wip: 'create_wip(arg0, arg1)',
  update_wip: 'update_wip(arg0, arg1, arg2)',
  delete_wip: 'delete_wip(arg0)',
  get_wip_json: 'get_wip_json(arg0)',
  get_wips_json: 'get_wip_json()',
  create_fp: 'create_fp(arg0, arg1)',
  update_fp: 'update_fp(arg0, arg1, arg2)',
  delete_fp: 'delete_fp(arg0)',
  get_fp_json: 'get_fp_json(arg0)',
  get_fps_json: 'get_fp_json()',
  create_activity: 'create_activity(arg0, arg1, arg2)',
  update_activity: 'update_activity(arg0, arg1, arg2, arg3)',
  delete_activity: 'delete_activity(arg0)',
  get_activity_json: 'get_activity_json(arg0)',
  get_activities_json: 'get_activity_json()',
  create_material: 'create_material(arg0, arg1, arg2, arg3, arg4, arg5)',
  update_material: 'update_material(arg0, arg1, arg2, arg3, arg4, arg5, arg6)',
  delete_material: 'delete_material(arg0)',
  get_material_json: 'get_material_json(arg0)',
  get_materials_json: 'get_materials_json()',
  get_item_registry_json: 'get_item_registry_json()',
});

export function createRJsonBridge(webR) {
  let pending = Promise.resolve();
  return function callJson(operation, ...args) {
    if (!Object.hasOwn(calls, operation)) throw new Error('Unknown R operation.');
    const env = {};
    args.forEach((value, index) => {
      if (typeof value !== 'string' && typeof value !== 'number') {
        throw new Error('R bridge inputs must be scalar strings or numbers.');
      }
      env[`arg${index}`] = value;
    });
    // Serialize calls; a rejected request must not block subsequent requests.
    const result = pending.then(async () => JSON.parse(
      await webR.evalRString(calls[operation], { env }),
    ));
    pending = result.catch(() => {});
    return result;
  };
}
