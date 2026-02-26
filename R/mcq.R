# =============================================================
# Interactive MCQ Generator for Quarto HTML
# Generates self-contained HTML/CSS/JS radio-button MCQs
# Answer is hidden until student clicks "Check Answer"
# =============================================================

#' Create an interactive MCQ with radio buttons
#'
#' @param id Unique identifier for this question (e.g., "m1q1")
#' @param question The question text (supports HTML)
#' @param options Named character vector. Name = option text, value = explanation.
#'   Prefix the correct answer's name with "answer:" to mark it correct.
#' @return HTML string (use with results='asis')
#'
make_mcq <- function(id, question, options) {

  # Parse correct answer
  correct_idx <- grep("^answer:", names(options))
  if (length(correct_idx) != 1) stop("Exactly one option must be prefixed with 'answer:'")

  # Clean option labels
  labels <- names(options)
  labels[correct_idx] <- sub("^answer:", "", labels[correct_idx])
  explanations <- unname(options)
  correct_letter <- LETTERS[correct_idx]

  # Build radio buttons — NO indentation, all tags flush left
  option_html <- ""
  for (i in seq_along(labels)) {
    letter <- LETTERS[i]
    option_html <- paste0(option_html,
      '<label class="mcq-option" id="', id, '-opt-', letter, '">',
      '<input type="radio" name="', id, '" value="', letter, '"> ',
      '<span class="mcq-letter">', letter, '.</span> ', labels[i],
      '</label>\n'
    )
  }

  # Build explanation items — NO indentation
  expl_html <- ""
  for (i in seq_along(labels)) {
    letter <- LETTERS[i]
    is_correct <- (i == correct_idx)
    icon <- if (is_correct) "&#10004;" else "&#10008;"
    cls <- if (is_correct) "mcq-expl-correct" else "mcq-expl-wrong"
    expl_html <- paste0(expl_html,
      '<div class="', cls, '"><strong>', icon, ' ', letter, '. ', labels[i],
      '</strong> &mdash; ', explanations[i], '</div>\n'
    )
  }

  # Build JS function
  js <- paste0(
'<script>
function checkMCQ_', id, '() {
var radios = document.getElementsByName("', id, '");
var selected = null;
for (var i = 0; i < radios.length; i++) {
if (radios[i].checked) { selected = radios[i].value; break; }
}
if (!selected) {
var fb = document.getElementById("', id, '-feedback");
fb.style.display = "block";
fb.className = "mcq-feedback mcq-feedback-warn";
fb.innerHTML = "Please select an option before checking.";
return;
}
var correct = "', correct_letter, '";
var fb = document.getElementById("', id, '-feedback");
var expl = document.getElementById("', id, '-explanation");
fb.style.display = "block";
expl.style.display = "block";
for (var i = 0; i < radios.length; i++) { radios[i].disabled = true; }
document.querySelector("#mcq-', id, ' .mcq-check-btn").disabled = true;
var opts = document.querySelectorAll("#mcq-', id, ' .mcq-option");
for (var i = 0; i < opts.length; i++) {
var val = opts[i].querySelector("input").value;
if (val === correct) { opts[i].classList.add("mcq-correct"); }
else if (val === selected) { opts[i].classList.add("mcq-wrong"); }
}
if (selected === correct) {
fb.className = "mcq-feedback mcq-feedback-correct";
fb.innerHTML = "&#10004; Correct!";
} else {
fb.className = "mcq-feedback mcq-feedback-wrong";
fb.innerHTML = "&#10008; Incorrect. The correct answer is " + correct + ".";
}
}
</script>'
  )

  # Full HTML block — all tags flush left, no leading whitespace
  html <- paste0(
'<div class="mcq-container" id="mcq-', id, '">\n',
'<div class="mcq-question"><strong>', question, '</strong></div>\n',
'<div class="mcq-options">\n',
option_html,
'</div>\n',
'<button class="mcq-check-btn" onclick="checkMCQ_', id, '()">Check Answer</button>\n',
'<div class="mcq-feedback" id="', id, '-feedback" style="display:none;"></div>\n',
'<div class="mcq-explanation" id="', id, '-explanation" style="display:none;">\n',
expl_html,
'</div>\n',
'</div>\n',
js
  )

  cat(html)
}
