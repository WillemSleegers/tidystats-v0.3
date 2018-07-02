function inspect_click(e) {
  // Figure out what was clicked
  var what = e.target.className;
  var identifier = "";
  var group = "";
  var term = "";
  var statistic = "";

  if (what == "") {
    what = "statistic";
  }

  // Figure out the identifier
  if (what == "identifier") {
    identifier = e.target.innerText;
  } else {

    // Get the previous row
    previous_row = e.target.parentNode.previousSibling.previousSibling;

    // Loop through all the rows until the row has a cell with the
    // identifier class
    while (previous_row.firstChild.className != "identifier") {
      previous_row = previous_row.previousSibling.previousSibling;
    }

    identifier = previous_row.firstChild.innerText;

    // Figure out the group
    if (what == "group") {
      group = e.target.innerText;
    } else {
      // Get the previous row
      previous_row = e.target.parentNode.previousSibling.previousSibling;

      // Loop through all the rows until the row has a child with the
      // group or identifier class
      while (previous_row.firstChild.className != "group" &&
        previous_row.firstChild.className != "identifier") {
        previous_row = previous_row.previousSibling.previousSibling;
      }

      if (previous_row.firstChild.className == "group") {
        group = previous_row.firstChild.innerText;
      }

      // Figure out the term
      if (what == "term") {
        term = e.target.innerText;
      } else {
      // Get the previous row
      previous_row = e.target.parentNode.previousSibling.previousSibling;
      // Loop through all the rows until the row has a child with the
      // term, group, or identifier class
      while (previous_row.firstChild.className != "group" &&
        previous_row.firstChild.className != "identifier" &&
        previous_row.firstChild.className != "term") {
        previous_row = previous_row.previousSibling.previousSibling;
      }

      // Check whether it is an identifier or group
      if (previous_row.firstChild.className == "term") {
        term = previous_row.firstChild.innerText;
      }

      // If a statistic was clicked, return the statistic
      if (what == "statistic") {
        statistic = e.target.parentNode.childNodes[1].innerText;
      }
      }
    }
  }

  //console.log("what: " + what);
  //console.log("identifier: " + identifier);
  //console.log("group: " + group);
  //console.log("term: " + term);
  //console.log("statistic: " + statistic);

  info = [what, identifier, group, term, statistic];
  console.log(info);

  Shiny.onInputChange("jsValue", info);

}