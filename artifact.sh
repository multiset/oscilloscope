ARTIFACT="artifact.tar.gz"
BUCKET="multiset-artifacts/oscilloscope"

tar -cvzf $ARTIFACT -C rel/osc .
aws s3 cp $ARTIFACT s3://$BUCKET/`git rev-parse HEAD`.tar.gz
aws s3 cp $ARTIFACT s3://$BUCKET/`git rev-parse --abbrev-ref HEAD`.tar.gz
