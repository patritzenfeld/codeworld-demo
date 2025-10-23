function showSettings() {
  document.getElementById("settingsArea").style.visibility = "visible";
  document.getElementById("templateArea").style.visibility = "hidden";
  document.getElementById("testArea").style.visibility = "hidden";
  setParam("Settings");
}

function showTemplate() {
  document.getElementById("settingsArea").style.visibility = "hidden";
  document.getElementById("templateArea").style.visibility = "visible";
  document.getElementById("testArea").style.visibility = "hidden";
  setParam("Template");
}

function showTests() {
  document.getElementById("settingsArea").style.visibility = "hidden";
  document.getElementById("templateArea").style.visibility = "hidden";
  document.getElementById("testArea").style.visibility = "visible";
  setParam("Tests");
}

function setParam(param) {
  if ('URLSearchParams' in window) {
    const url = new URL(window.location)
    url.searchParams.set("visible", param)
    history.pushState(null, '', url);
  }
}
