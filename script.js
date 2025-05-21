/**
 * Add a listener to the DataTable to watch for changes in the second column.
 * When a change is detected, update the corresponding cell and the data in shiny.
 * 
 * @param {string} tableId DT table ID.
 * @param {string} tableInputId ID of the shiny input to store the data.
 */
function watchDTCellChange(tableId, tableInputId) {
  const table = $("#" + tableId + " table").DataTable();
  if (table) {
    table.on('change', 'input', function() {
      const cell = table.cell($(this).closest('td'));
      const row = cell.index().row;
      const col = cell.index().column;
      if (col !== 1) {
        return; // Only watch changes in the second column (index 1)
      }
      const newVal = $(this).val();
      cell.data(newVal); // Update the cell with the new value
      // Want to update the `Action` column when then change the cell
      const actionCell = table.cell({ row: row, column: 2 });
      actionCell.data("Changed by user");
      // Return the input holding current data in shiny
      getCurrentDTData(tableId, tableInputId);
    });
  }
}

/**
 * Get the current data from the DataTable and send it to shiny.
 * 
 * @param {string} tableId DT table ID.
 * @param {string} tableInputId ID of the shiny input to store the data.
 */
function getCurrentDTData(tableId, tableInputId) {
  const table = $("#" + tableId + " table").DataTable();
  if (table) {
    const data = table.rows().data().toArray();
    const jsonData = data.map(row => ({
      original: row[0],
      renamed: row[1],
      action: row[2]
    }));
    Shiny.setInputValue(tableInputId, JSON.stringify(jsonData));
  }
}

/**
 * Add/remove a class to the modal footer that will allow the button to be clickable or not.
 *
 * Currently, if either the "microorganism-close-disabled" or "antibacterial-close-disabled" class
 * is present, the button will be disabled.
 *
 * @param {string} changeLogType The type of change log (i.e., "microorganism", "antibacterial").
 * @param {boolean} enable Enable or disable the modal close button.
 */
function enableModalClose(changeLogType, enable = true) {
  const modalCloseFooter = document.getElementById("import-modal-footer");
  if (enable) {
    modalCloseFooter.classList.remove(`${changeLogType}-close-disabled`);
  } else {
    modalCloseFooter.classList.add(`${changeLogType}-close-disabled`);
  }
}
