---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults
layout: home
title: Home
---
<link href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css" rel="stylesheet">

<style type="text/css">
  /* Font Awesome icons */
  .fab, .fas {
    padding: 7px;
  }
  .fab:hover, .fas:hover {
    opacity: 0.7;
  }

  /* Two-column hero: bio on the left, profile on the right */
  .hero {
    display: flex;
    flex-wrap: wrap;
    gap: 2em;
    align-items: flex-start;
  }
  .hero__bio {
    flex: 1 1 360px;
  }
  .hero__profile {
    flex: 0 1 300px;
    background-color: #f7f7f7;
    padding: 1.5em;
    border-radius: 6px;
    text-align: center;
  }
  .hero__profile img {
    max-width: 100%;
    height: auto;
    border-radius: 50%;
  }
  .hero__profile h2 {
    margin: 0.5em 0 0.25em;
    font-size: 1.6em;
  }
  .hero__profile p {
    margin: 0 0 0.75em;
    font-size: 0.95em;
  }

  /* Interests / Education sub-columns */
  .info-row {
    display: flex;
    flex-wrap: wrap;
    gap: 1.5em;
    margin-top: 1em;
  }
  .info-row > div {
    flex: 1 1 220px;
  }
</style>

<div class="hero">
  <div class="hero__bio">
    <p>
      Hi, my name is Lucas!
    </p>
    <p>
      I'm a postdoctoral researcher in Applied Psychology at the Department of Psychology at the University of Innsbruck, Austria, where I also  completed my dissertation in Work and Organizational Psychology. My research focuses on occupational well-being, health, motivation, and identity — topics I explore empirically using longitudinal data modeling and experience sampling methods.
    </p>
    <p>
      I am a strong proponent of open and collaborative science. Next to applied psychology I am very much interested in research methods. In my free time I like to tinker with hardware, software, and coding.
    </p>
    <p>
      <a href="/download/CV-Lucas-Maunz.pdf" title="My CV, last update 05.2026" download>
        <i class="fa fa-download" aria-hidden="true"></i> My CV (last update 05.26)
      </a>
    </p>

    <div class="info-row">
      <div>
        <h3>Interests</h3>
        <ul>
          <li>Occupational well-being, health, and motivation</li>
          <li>Identity and its development at work</li>
          <li>Research methods</li>
        </ul>
      </div>
      <div>
        <h3>Education</h3>
        <ul>
          <li>PhD in Psychology, 2020–2024, University of Innsbruck</li>
          <li>BSc. and MSc. in Psychology, 2012–2020, University of Innsbruck</li>
        </ul>
      </div>
    </div>
  </div>

  <div class="hero__profile">
    <img src="/assets/images/profile/me2.png" alt="Lucas Maunz" />
    <h2>Lucas Maunz</h2>
    <p>Postdoctoral Researcher in Applied Psychology<br>University of Innsbruck</p>
    <a href="https://www.researchgate.net/profile/Lucas-Maunz/" class="fab fa-researchgate fa-2x" aria-label="ResearchGate"></a>
    <a href="https://github.com/semluc/" class="fab fa-github fa-2x" aria-label="GitHub"></a>
    <a href="mailto:lucas.maunz@uibk.ac.at" class="fas fa-envelope fa-2x" aria-label="Email"></a>
    <a href="https://www.uibk.ac.at/psychologie/mitarbeiter/maunz/" class="fas fa-university fa-2x" aria-label="University page"></a>
  </div>
</div>
