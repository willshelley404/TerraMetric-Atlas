"""Email the outcome of the recession-model workflow.

All inputs come from environment variables set by the workflow. Without the
three SMTP/recipient secrets it exits quietly, so the workflow works before
email is configured. Set DRY_RUN=1 to print the message instead of sending.
"""
import os
import smtplib
import sys
from email.message import EmailMessage

e = os.environ

if not (e.get("SMTP_USER") and e.get("SMTP_PASSWORD") and e.get("NOTIFY_EMAIL")):
    print("Email secrets not set; skipping notification.")
    sys.exit(0)

ok = e["BUILD_RESULT"] == "success" and e["DEPLOY_RESULT"] == "success"
if ok:
    subject = f"Recession model updated: {e['PROB']}% ({e['MONTH']})"
    body = (
        "The recession model document was rebuilt and published.\n\n"
        f"Month scored:  {e['MONTH']}\n"
        f"Probability:   {e['PROB']}%  ({e['TIER']})\n\n"
        f"Document: {e['SITE_URL']}\n"
        f"Run log:  {e['RUN_URL']}\n"
    )
else:
    subject = "Recession model document FAILED to update"
    body = (
        f"Build:  {e['BUILD_RESULT']}\n"
        f"Deploy: {e['DEPLOY_RESULT']}\n\n"
        "The previously published version is still live.\n"
        f"Run log: {e['RUN_URL']}\n"
    )

msg = EmailMessage()
msg["Subject"] = subject
msg["From"] = e["SMTP_USER"]
msg["To"] = e["NOTIFY_EMAIL"]
msg.set_content(body)

if e.get("DRY_RUN"):
    print(msg)
    sys.exit(0)

with smtplib.SMTP_SSL("smtp.gmail.com", 465) as s:
    s.login(e["SMTP_USER"], e["SMTP_PASSWORD"])
    s.send_message(msg)
print("Notification sent.")
