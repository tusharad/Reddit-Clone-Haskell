function toggle() {
  var elems = document.getElementsByClassName('replyBox');
  Array.from(elems).forEach((x) => {
    if (x.style.display === "none") {
      x.style.display = "block";
    } else {
      x.style.display = "none";
    }
  })
}
