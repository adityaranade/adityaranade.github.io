---
title: "Loan Schedule"
subtitle: "Let us understand the components of EMI"
author: "Aditya Ranade"
highlight-style:
            light: github
date: "2026-01-13"
categories: [analysis, R]
image: "./emi.png"
format: html
---

::: {style="text-align: justify"}
We generally buy an asset on loan. In order to repay the loan, we have to pay Equated Monthly Instalment (EMI). Every EMI payment contains a principal component and interest component. Let us look at the amortization of a loan. For initial calculation, we will keep the loan amount as 1,00,00,000; interest rate of 7.5% and tenure of 20 years with monthly payments.

:::

<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

<style>
  body {
    font-family: Arial, sans-serif;
    max-width: 1100px;
    margin: auto;
    padding: 20px;
  }
  input, select, button {
    margin: 5px;
    padding: 5px;
  }
  table {
    border-collapse: collapse;
    width: 100%;
    margin-top: 15px;
  }
  th, td {
    border: 1px solid #aaa;
    padding: 6px;
    text-align: center;
  }
  th {
    background: #f2f2f2;
  }
  #plots {
    display: flex;
    flex-wrap: wrap;
    gap: 20px;
  }
  .plotBox {
    width: 48%;
    min-width: 300px;
  }
</style>

<h2>Loan EMI & Amortization Calculator</h2>

<div>
  <label>Loan Amount:</label>
  <input type="number" id="loan" value="10000000"><br>

  <label>Annual Interest Rate (%):</label>
  <input type="number" id="rate" value="7.5"><br>

  <label>Tenure (Years):</label>
  <input type="number" id="years" value="20"><br>

  <label>Payments per Year:</label>
  <select id="freq">
    <option value="12">Monthly</option>
    <option value="4">Quarterly</option>
    <option value="2">Semi-Annual</option>
    <option value="1">Annual</option>
  </select>

  <br><br>
  <button onclick="update()">Calculate</button>
</div>

<h3 id="emi"></h3>

<div id="plots">
  <div id="balancePlot" class="plotBox"></div>
  <div id="ipPlot" class="plotBox"></div>
</div>

<h3>Amortization Schedule</h3>
<table id="scheduleTable"></table>

<script>
// ---- EMI Logic ----
function loanSchedule(P, annualRate, years, freq) {
  const n = years * freq;
  const r = annualRate / 100 / freq;

  let payment;
  if (r === 0) {
    payment = P / n;
  } else {
    payment = P * r * Math.pow(1 + r, n) / (Math.pow(1 + r, n) - 1);
  }

  let schedule = [];
  let balance = P;

  for (let i = 1; i <= n; i++) {
    const interest = balance * r;
    const principal = payment - interest;
    const closing = balance - principal;
    const percEmi = (interest / payment) * 100;

    schedule.push({
      period: i,
      opening: Math.round(balance),
      payment: Math.round(payment),
      interest: Math.round(interest),
      principal: Math.round(principal),
      percEmi: percEmi.toFixed(2),
      closing: Math.round(closing)
    });

    balance = closing;
  }

  return {payment: Math.round(payment), table: schedule};
}

// ---- Table ----
function renderTable(data) {
  let html = "<tr>\
    <th>Period</th>\
    <th>Opening Balance</th>\
    <th>Payment</th>\
    <th>Interest</th>\
    <th>Principal</th>\
    <th>Interest (% of EMI)</th>\
    <th>Closing Balance</th>\
    </tr>";

  data.forEach(row => {
    html += `<tr>
      <td>${row.period}</td>
      <td>${row.opening}</td>
      <td>${row.payment}</td>
      <td>${row.interest}</td>
      <td>${row.principal}</td>
      <td>${row.percEmi}</td>
      <td>${row.closing}</td>
    </tr>`;
  });

  document.getElementById("scheduleTable").innerHTML = html;
}

// ---- Plots ----
function renderPlots(table) {
  const periods = table.map(r => r.period);
  const closing = table.map(r => r.closing);
  const interest = table.map(r => r.interest);
  const principal = table.map(r => r.principal);

  Plotly.newPlot("balancePlot", [{
    x: periods, y: closing, type: "scatter", mode: "lines"
  }], {title: "Outstanding Balance"});

  Plotly.newPlot("ipPlot", [
    {x: periods, y: interest, type: "scatter", mode: "lines", name: "Interest"},
    {x: periods, y: principal, type: "scatter", mode: "lines", name: "Principal"}
  ], {title: "Interest vs Principal"});
}

// ---- Update ----
function update() {
  const P = Number(document.getElementById("loan").value);
  const rate = Number(document.getElementById("rate").value);
  const years = Number(document.getElementById("years").value);
  const freq = Number(document.getElementById("freq").value);

  const result = loanSchedule(P, rate, years, freq);

  document.getElementById("emi").innerText =
    "Payment per period = " + result.payment;

  renderTable(result.table);
  renderPlots(result.table);
}

window.onload = update;
</script>


::: {style="text-align: justify"}
For loan amount 1,00,00,000; interest rate of 7.5% and tenure of 20 years with monthly payments, the EMI comes out to be 80559. In the very first payment, the interest component is 62500 and principal component is of 18059. The interest component is 77.58% of the total payment of the first payment.In every subsequent payment, the interest component % (of the total payment) goes down and the principal component % (of the total payment) goes up. If we look at installment number 130, it is the first time the principal component is higher than the interest component. This means for the first 129 months (10 years and 9 months), majority of the monthly payment goes towards the interest payment. This will change if the amount, interest rate, tenure and frequency of payment changes. To get your specific EMI, you can change amount, interest rate, tenure and frequency of payment (in which case it will not be technically Equated Monthly Installments but it will be equated installments) .
:::